// Copyright 2011 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package time

import (
	"errors"
	"sync"
	"syscall"
)

//go:generate env ZONEINFO=$GOROOT/lib/time/zoneinfo.zip go run genzabbrs.go -output zoneinfo_abbrs_windows.go

// A Location maps time instants to the zone in use at that time.
// Typically, the Location represents the collection of time offsets
// in use in a geographical area. For many Locations the time offset varies
// depending on whether daylight savings time is in use at the time instant.
//
// Location is used to provide a time zone in a printed Time value and for
// calculations involving intervals that may cross daylight savings time
// boundaries.
type Location struct {
	name string
	zone []zone
	tx   []zoneTrans

	// The tzdata information can be followed by a string that describes
	// how to handle DST transitions after the last zoneTrans.
	// The format is the TZ environment variable without a colon; see
	// https://pubs.opengroup.org/onlinepubs/9699919799/basedefs/V1_chap08.html.
	// Example string, for America/Los_Angeles: PST8PDT,M3.2.0,M11.1.0
	rule []rule

	// Most lookups will be for the current time.
	// To avoid the binary search through tx, keep a
	// static one-element cache that gives the correct
	// zone for the time when the Location was created.
	// if cacheStart <= t < cacheEnd,
	// lookup can return cacheZone.
	// The units for cacheStart and cacheEnd are seconds
	// since January 1, 1970 UTC, to match the argument
	// to lookup.
	cacheStart int64
	cacheEnd   int64
	cacheZone  *zone
}

// A zone represents a single time zone such as CET.
type zone struct {
	name   string // abbreviated name, "CET"
	offset int    // seconds east of UTC
	isDST  bool   // is this zone Daylight Savings Time?
}

// A zoneTrans represents a single time zone transition.
type zoneTrans struct {
	when         int64 // transition time, in seconds since 1970 GMT
	index        uint8 // the index of the zone that goes into effect at that time
	isstd, isutc bool  // ignored - no idea what these mean
}

// alpha and omega are the beginning and end of time for zone
// transitions.
const (
	alpha = -1 << 63  // math.MinInt64
	omega = 1<<63 - 1 // math.MaxInt64
)

// UTC represents Universal Coordinated Time (UTC).
var UTC *Location = &utcLoc

// utcLoc is separate so that get can refer to &utcLoc
// and ensure that it never returns a nil *Location,
// even if a badly behaved client has changed UTC.
var utcLoc = Location{name: "UTC"}

// Local represents the system's local time zone.
// On Unix systems, Local consults the TZ environment
// variable to find the time zone to use. No TZ means
// use the system default /etc/localtime.
// TZ="" means use UTC.
// TZ="foo" means use file foo in the system timezone directory.
var Local *Location = &localLoc

// localLoc is separate so that initLocal can initialize
// it even if a client has changed Local.
var localLoc Location
var localOnce sync.Once

func (l *Location) get() *Location {
	if l == nil {
		return &utcLoc
	}
	if l == &localLoc {
		localOnce.Do(initLocal)
	}
	return l
}

// String returns a descriptive name for the time zone information,
// corresponding to the name argument to LoadLocation or FixedZone.
func (l *Location) String() string {
	return l.get().name
}

var unnamedFixedZones []*Location
var unnamedFixedZonesOnce sync.Once

// FixedZone returns a Location that always uses
// the given zone name and offset (seconds east of UTC).
func FixedZone(name string, offset int) *Location {
	// Most calls to FixedZone have an unnamed zone with an offset by the hour.
	// Optimize for that case by returning the same *Location for a given hour.
	const hoursBeforeUTC = 12
	const hoursAfterUTC = 14
	hour := offset / 60 / 60
	if name == "" && -hoursBeforeUTC <= hour && hour <= +hoursAfterUTC && hour*60*60 == offset {
		unnamedFixedZonesOnce.Do(func() {
			unnamedFixedZones = make([]*Location, hoursBeforeUTC+1+hoursAfterUTC)
			for hr := -hoursBeforeUTC; hr <= +hoursAfterUTC; hr++ {
				unnamedFixedZones[hr+hoursBeforeUTC] = fixedZone("", hr*60*60)
			}
		})
		return unnamedFixedZones[hour+hoursBeforeUTC]
	}
	return fixedZone(name, offset)
}

func fixedZone(name string, offset int) *Location {
	l := &Location{
		name:       name,
		zone:       []zone{{name, offset, false}},
		tx:         []zoneTrans{{alpha, 0, false, false}},
		cacheStart: alpha,
		cacheEnd:   omega,
	}
	l.cacheZone = &l.zone[0]
	return l
}

// lookup returns information about the time zone in use at an
// instant in time expressed as seconds since January 1, 1970 00:00:00 UTC.
//
// The returned information gives the name of the zone (such as "CET"),
// the start and end times bracketing sec when that zone is in effect,
// the offset in seconds east of UTC (such as -5*60*60), and whether
// the daylight savings is being observed at that time.
func (l *Location) lookup(sec int64) (name string, offset int, start, end int64, isDST bool) {
	l = l.get()

	if len(l.zone) == 0 {
		name = "UTC"
		offset = 0
		start = alpha
		end = omega
		isDST = false
		return
	}

	if zone := l.cacheZone; zone != nil && l.cacheStart <= sec && sec < l.cacheEnd {
		name = zone.name
		offset = zone.offset
		start = l.cacheStart
		end = l.cacheEnd
		isDST = zone.isDST
		return
	}

	// Binary search for entry with largest time <= sec.
	// Not using sort.Search to avoid dependencies.
	tx := l.tx
	end = omega
	lo := 0
	hi := len(tx)
	for hi-lo > 1 {
		m := int(uint(lo+hi) >> 1)
		lim := tx[m].when
		if sec < lim {
			end = lim
			hi = m
		} else {
			lo = m
		}
	}
	zone := &l.zone[tx[lo].index]
	name = zone.name
	offset = zone.offset
	start = tx[lo].when
	// end = maintained during the search
	isDST = zone.isDST

	// If we're at the end of the known zone transitions,
	// try the extend string.
	if lo == len(tx)-1 {
		if ezone, estart, eend := tzrule(l.rule, start, sec); ezone != nil {
			return ezone.name, ezone.offset, estart, eend, ezone.isDST
		}
	}

	return
}

// lookupFirstZone returns the index of the time zone to use for times
// before the first transition time, or when there are no transition
// times.
//
// The reference implementation in localtime.c from
// https://www.iana.org/time-zones/repository/releases/tzcode2013g.tar.gz
// implements the following algorithm for these cases:
//  1. If the first zone is unused by the transitions, use it.
//  2. Otherwise, if there are transition times, and the first
//     transition is to a zone in daylight time, find the first
//     non-daylight-time zone before and closest to the first transition
//     zone.
//  3. Otherwise, use the first zone that is not daylight time, if
//     there is one.
//  4. Otherwise, use the first zone.
func (l *Location) lookupFirstZone() int {
	// Case 1.
	if !l.firstZoneUsed() {
		return 0
	}

	// Case 2.
	if len(l.tx) > 1 && l.zone[l.tx[1].index].isDST {
		for zi := int(l.tx[1].index) - 1; zi >= 0; zi-- {
			if !l.zone[zi].isDST {
				return zi
			}
		}
	}

	// Case 3.
	for zi := range l.zone {
		if !l.zone[zi].isDST {
			return zi
		}
	}

	// Case 4.
	return 0
}

// firstZoneUsed reports whether the first zone is used by some
// transition.
func (l *Location) firstZoneUsed() bool {
	for _, tx := range l.tx[1:] {
		if tx.index == 0 {
			return true
		}
	}
	return false
}

// tzset parses a timezone string like the one found in the TZ environment
// variable.  We call this a tzset string since in C the function tzset
// reads TZ.  The return values are transition rules, target zones,
// plus ok which reports whether the parse succeeded.  If the string
// specifies a static zone or a DST zone spanning the whole year, the zone
// is returned in newYearZone and rules is nil.  Otherwise rules is of
// length 2.  Zones are returned as values to avoid unneeded allocation.
func tzset(s string) (rules []rule, newYearZone, midYearZone zone, ok bool) {
	newYearZone.name, s, ok = tzsetName(s)
	if ok {
		newYearZone.offset, s, ok = tzsetOffset(s, 24)
	}
	if !ok {
		newYearZone = zone{}
		return
	}

	// The numbers in the tzset string are added to local time to get UTC,
	// but our offsets are added to UTC to get local time,
	// so we negate the number we see here.
	newYearZone.offset = -newYearZone.offset

	if len(s) == 0 || s[0] == ',' {
		// No daylight savings time.
		return
	}

	midYearZone.name, s, ok = tzsetName(s)
	if ok {
		if len(s) == 0 || s[0] == ',' {
			midYearZone.offset = newYearZone.offset + secondsPerHour
		} else {
			midYearZone.offset, s, ok = tzsetOffset(s, 24)
			midYearZone.offset = -midYearZone.offset // as with newYearZone.offset, above
		}
	}
	if !ok {
		return nil, zone{}, zone{}, false
	}
	midYearZone.isDST = true

	if len(s) == 0 {
		// Default DST rules per tzcode.
		s = ",M3.2.0,M11.1.0"
	}
	// The TZ definition does not mention ';' here but tzcode accepts it.
	if s[0] != ',' && s[0] != ';' {
		return nil, zone{}, zone{}, false
	}
	s = s[1:]

	rules = make([]rule, 2)
	rules[0], s, ok = tzsetRule(s)
	if !ok || len(s) == 0 || s[0] != ',' {
		return nil, zone{}, zone{}, false
	}
	s = s[1:]
	rules[1], s, ok = tzsetRule(s)
	if !ok || len(s) > 0 {
		return nil, zone{}, zone{}, false
	}

	// Convert time to UTC.
	rules[0].time -= newYearZone.offset
	rules[1].time -= midYearZone.offset

	// Zoneinfo version 3 supports zones with permanent DST.
	// Check the length of DST in a leap year to detect this case.
	// Example string: STD0DST0,J1/0,J365/24
	if rules[0].dow < 0 && rules[1].dow < 0 {
		startSec := tzruleTime(0, rules[0], true)
		endSec := tzruleTime(0, rules[1], true)
		if endSec-startSec >= 366*secondsPerDay {
			return nil, midYearZone, zone{}, true
		}
	}

	rules[0].normalize()
	rules[1].normalize()

	// Reorder rules and zones in order they occur in a year.
	if rules[0].day > rules[1].day {
		rules[0], rules[1] = rules[1], rules[0]
		newYearZone, midYearZone = midYearZone, newYearZone
	}
	return
}

// normalize changes r to the functionally equivalent canonical form
// so that its time is within [0, secondsPerDay)
// and its day is non-negative and, if possible, below 365.
//
// The normalized rule may result in transitions after the end of the year if:
//   - it is a day-of-year rule (addLeapDay = false) whose day,
//     after being adjusted according to its time, is >= 365;
//   - it is a month-week-day rule whose adjusted day is 26 to 31 December.
func (r *rule) normalize() {
	if uint(r.time) >= secondsPerDay {
		var d int
		d, r.time = norm(0, r.time, secondsPerDay)
		r.day += d
		if r.dow >= 0 {
			_, r.dow = norm(0, r.dow+d, 7)
		}
		if !r.addLeapDay && r.day < 0 {
			r.day += 365
			r.addLeapDay = true
		} else if r.addLeapDay && r.day >= 365 {
			r.day -= 365
			r.addLeapDay = false
		}
	}
}

// tzsetName returns the timezone name at the start of the tzset string s,
// and the remainder of s, and reports whether the parsing is OK.
func tzsetName(s string) (string, string, bool) {
	if len(s) == 0 {
		return "", "", false
	}
	if s[0] != '<' {
		for i, r := range s {
			switch r {
			case '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', ',', '-', '+':
				if i < 3 {
					return "", "", false
				}
				return s[:i], s[i:], true
			}
		}
		if len(s) < 3 {
			return "", "", false
		}
		return s, "", true
	} else {
		for i, r := range s {
			if r == '>' {
				return s[1:i], s[i+1:], true
			}
		}
		return "", "", false
	}
}

// tzsetOffset returns the timezone offset at the start of the tzset string s,
// and the remainder of s, and reports whether the parsing is OK.
// The timezone offset is returned as a number of seconds.
func tzsetOffset(s string, maxHour int) (offset int, rest string, ok bool) {
	if len(s) == 0 {
		return 0, "", false
	}
	neg := false
	if s[0] == '+' {
		s = s[1:]
	} else if s[0] == '-' {
		s = s[1:]
		neg = true
	}

	var hours int
	hours, s, ok = tzsetNum(s, 0, maxHour)
	if !ok {
		return 0, "", false
	}
	off := hours * secondsPerHour
	if len(s) == 0 || s[0] != ':' {
		if neg {
			off = -off
		}
		return off, s, true
	}

	var mins int
	mins, s, ok = tzsetNum(s[1:], 0, 59)
	if !ok {
		return 0, "", false
	}
	off += mins * secondsPerMinute
	if len(s) == 0 || s[0] != ':' {
		if neg {
			off = -off
		}
		return off, s, true
	}

	var secs int
	secs, s, ok = tzsetNum(s[1:], 0, 59)
	if !ok {
		return 0, "", false
	}
	off += secs

	if neg {
		off = -off
	}
	return off, s, true
}

// rule is a rule read from a tzset string.
type rule struct {
	day        int   // day of year of transition, zero-origin
	dow        int   // day of week (0=Sunday) or -1 for any
	addLeapDay bool  // add a day in leap years?
	time       int   // time of day of transition in UTC
	zone       *zone // transition target
}

// tzsetRule parses a rule from a tzset string.
// It returns the rule, and the remainder of the string, and reports success.
func tzsetRule(s string) (rule, string, bool) {
	var r rule
	if len(s) == 0 {
		return rule{}, "", false
	}
	ok := false
	if s[0] == 'J' {
		var jday int
		jday, s, ok = tzsetNum(s[1:], 1, 365)
		if !ok {
			return rule{}, "", false
		}
		jday--
		addLeapDay := jday >= 31+28
		r.day = jday
		r.dow = -1
		r.addLeapDay = addLeapDay
	} else if s[0] == 'M' {
		var mon int
		mon, s, ok = tzsetNum(s[1:], 1, 12)
		if !ok || len(s) == 0 || s[0] != '.' {
			return rule{}, "", false

		}
		var week int
		week, s, ok = tzsetNum(s[1:], 1, 5)
		if !ok || len(s) == 0 || s[0] != '.' {
			return rule{}, "", false
		}
		var dow int
		dow, s, ok = tzsetNum(s[1:], 0, 6)
		if !ok {
			return rule{}, "", false
		}
		// Convert to 0-based weeks, treating week 5 as week -1
		// of the next month.
		week--
		mon += week >> 2        // 1 <= mon <= 13
		week = week&3 - week>>2 // -1 <= week <= 3
		day := int(daysBefore[mon-1]) + week*7
		addLeapDay := mon > 2
		r.day = day
		r.dow = dow
		r.addLeapDay = addLeapDay
	} else {
		var day int
		day, s, ok = tzsetNum(s, 0, 365)
		if !ok {
			return rule{}, "", false
		}
		r.day = day
		r.dow = -1
	}

	if len(s) == 0 || s[0] != '/' {
		r.time = 2 * secondsPerHour // 2am is the default
		return r, s, true
	}

	// The tzdata code permits values up to 24 * 7 - 1 here,
	// although POSIX does not.
	offset, s, ok := tzsetOffset(s[1:], 24*7-1)
	if !ok {
		return rule{}, "", false
	}
	r.time = offset

	return r, s, true
}

// tzsetNum parses a number from a tzset string.
// It returns the number, and the remainder of the string, and reports success.
// The number must be between min and max.
func tzsetNum(s string, min, max int) (num int, rest string, ok bool) {
	if len(s) == 0 {
		return 0, "", false
	}
	num = 0
	for i, r := range s {
		if r < '0' || r > '9' {
			if i == 0 || num < min {
				return 0, "", false
			}
			return num, s[i:], true
		}
		num *= 10
		num += int(r) - '0'
		if num > max {
			return 0, "", false
		}
	}
	if num < min {
		return 0, "", false
	}
	return num, "", true
}

// If rules are present, tzrule returns the zone that applies at sec
// and the time span when the zone is in effect.
// Otherwise it returns nil and the time span from lastTxSec to omega.
func tzrule(rule []rule, lastTxSec, sec int64) (z *zone, start, end int64) {
	if len(rule) != 2 {
		return nil, lastTxSec, omega
	}

	year, _, _, yday := absDate(uint64(sec+unixToInternal+internalToAbsolute), false)

	// Compute start of year in days since absolute epoch.
	d := daysSinceEpoch(year)
	leap := isLeap(year)

	// If sec is guaranteed to be before the second transition in the
	// year, calculate the time of the first transition, otherwise of
	// the second.
	halfYear := year << 1
	if yday >= rule[1].day {
		halfYear++
	}
	start = tzruleTime(d, rule[halfYear&1], leap)
	// Calculate previous or next transition.
	if sec < start {
		// This loop will run twice if last year's
		// second transition occurs in this year after sec.
		for sec < start {
			end = start
			if halfYear--; halfYear&1 != 0 {
				leap = !leap && isLeap(halfYear>>1)
				if leap {
					d -= 366
				} else {
					d -= 365
				}
			}
			start = tzruleTime(d, rule[halfYear&1], leap)
		}
		z = rule[halfYear&1].zone
	} else {
		z = rule[halfYear&1].zone
		if halfYear++; halfYear&1 == 0 {
			if leap {
				d += 366
			} else {
				d += 365
			}
			leap = !leap && isLeap(halfYear>>1)
		}
		end = tzruleTime(d, rule[halfYear&1], leap)
	}

	// Ensure that start is not before the last known transition.
	// end is guaranteed to be after.
	if start < lastTxSec {
		start = lastTxSec
	}
	return
}

// tzruleTime takes the start of a year in days since absolute epoch,
// a rule, and a leap year flag, and returns the Unix time
// when the rule takes effect.
func tzruleTime(yearDay uint64, r rule, leapYear bool) int64 {
	d := int(yearDay) + r.day
	if leapYear && r.addLeapDay {
		d++
	}
	if r.dow >= 0 {
		// Find the day of week r.dow in the week starting on day d.
		// Day 0 is Monday, therefore d%7 is the day of week of
		// d-1 or d+6.  To speed up the calculation, subtract the
		// reverse difference from d+6.  NOTE(vadik): correctness
		// guaranteed only from 7 January absoluteZeroYear.
		delta := (d - r.dow) % 7
		d += 6 - delta
	}
	return int64(d*secondsPerDay+r.time) + (absoluteToInternal + internalToUnix)
}

// lookupName returns information about the time zone with
// the given name (such as "EST") at the given pseudo-Unix time
// (what the given time of day would be in UTC).
func (l *Location) lookupName(name string, unix int64) (offset int, ok bool) {
	l = l.get()

	// First try for a zone with the right name that was actually
	// in effect at the given time. (In Sydney, Australia, both standard
	// and daylight-savings time are abbreviated "EST". Using the
	// offset helps us pick the right one for the given time.
	// It's not perfect: during the backward transition we might pick
	// either one.)
	for i := range l.zone {
		zone := &l.zone[i]
		if zone.name == name {
			nam, offset, _, _, _ := l.lookup(unix - int64(zone.offset))
			if nam == zone.name {
				return offset, true
			}
		}
	}

	// Otherwise fall back to an ordinary name match.
	for i := range l.zone {
		zone := &l.zone[i]
		if zone.name == name {
			return zone.offset, true
		}
	}

	// Otherwise, give up.
	return
}

// NOTE(rsc): Eventually we will need to accept the POSIX TZ environment
// syntax too, but I don't feel like implementing it today.

var errLocation = errors.New("time: invalid location name")

var zoneinfo *string
var zoneinfoOnce sync.Once

// LoadLocation returns the Location with the given name.
//
// If the name is "" or "UTC", LoadLocation returns UTC.
// If the name is "Local", LoadLocation returns Local.
//
// Otherwise, the name is taken to be a location name corresponding to a file
// in the IANA Time Zone database, such as "America/New_York".
//
// LoadLocation looks for the IANA Time Zone database in the following
// locations in order:
//
//   - the directory or uncompressed zip file named by the ZONEINFO environment variable
//   - on a Unix system, the system standard installation location
//   - $GOROOT/lib/time/zoneinfo.zip
//   - the time/tzdata package, if it was imported
func LoadLocation(name string) (*Location, error) {
	if name == "" || name == "UTC" {
		return UTC, nil
	}
	if name == "Local" {
		return Local, nil
	}
	if containsDotDot(name) || name[0] == '/' || name[0] == '\\' {
		// No valid IANA Time Zone name contains a single dot,
		// much less dot dot. Likewise, none begin with a slash.
		return nil, errLocation
	}
	zoneinfoOnce.Do(func() {
		env, _ := syscall.Getenv("ZONEINFO")
		zoneinfo = &env
	})
	var firstErr error
	if *zoneinfo != "" {
		if zoneData, err := loadTzinfoFromDirOrZip(*zoneinfo, name); err == nil {
			if z, err := LoadLocationFromTZData(name, zoneData); err == nil {
				return z, nil
			}
			firstErr = err
		} else if err != syscall.ENOENT {
			firstErr = err
		}
	}
	if z, err := loadLocation(name, platformZoneSources); err == nil {
		return z, nil
	} else if firstErr == nil {
		firstErr = err
	}
	return nil, firstErr
}

// containsDotDot reports whether s contains "..".
func containsDotDot(s string) bool {
	if len(s) < 2 {
		return false
	}
	for i := 0; i < len(s)-1; i++ {
		if s[i] == '.' && s[i+1] == '.' {
			return true
		}
	}
	return false
}

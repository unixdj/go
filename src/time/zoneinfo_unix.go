// Copyright 2009 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

//go:build unix && !ios && !android

// Parse "zoneinfo" time zone file.
// This is a fairly standard file format used on OS X, Linux, BSD, Sun, and others.
// See tzfile(5), https://en.wikipedia.org/wiki/Zoneinfo,
// and ftp://munnari.oz.au/pub/oldtz/

package time

import (
	"errors"
	"syscall"
)

// Many systems use /usr/share/zoneinfo, Solaris 2 has
// /usr/share/lib/zoneinfo, IRIX 6 has /usr/lib/locale/TZ,
// NixOS has /etc/zoneinfo.
var platformZoneSources = []string{
	"/usr/share/zoneinfo/",
	"/usr/share/lib/zoneinfo/",
	"/usr/lib/locale/TZ/",
	"/etc/zoneinfo",
}

func initLocalFromTZString(tz string) bool {
	rule, newYearZone, midYearZone, ok := tzset(tz)
	if ok {
		localLoc = Location{
			name: "Local",
			tx:   []zoneTrans{{alpha, 0, false, false}},
			rule: rule,
		}
		if len(rule) == 2 {
			localLoc.zone = []zone{newYearZone, midYearZone}
			rule[0].zone = &localLoc.zone[1]
			rule[1].zone = &localLoc.zone[0]
			sec, _, _ := now()
			localLoc.cacheZone, localLoc.cacheStart, localLoc.cacheEnd = tzrule(rule, alpha, sec)
		} else {
			localLoc.zone = []zone{newYearZone}
			localLoc.cacheStart = alpha
			localLoc.cacheEnd = omega
			localLoc.cacheZone = &localLoc.zone[0]
		}
	}
	return ok
}

func initLocal() {
	// consult $TZ to find the time zone to use.
	// no $TZ means use the system default /etc/localtime.
	// $TZ="" means use UTC.
	// $TZ=":foo" if foo is an absolute path, then the file pointed
	// by foo will be used to initialize timezone; otherwise, file
	// /usr/share/zoneinfo/foo will be used.
	// $TZ="foo" likewise, but if the file is not found, the POSIX
	// style TZ string foo will be used to initialize timezone.

	tz, ok := syscall.Getenv("TZ")
	switch {
	case !ok:
		z, err := loadLocation("localtime", []string{"/etc"})
		if err == nil {
			localLoc = *z
			localLoc.name = "Local"
			return
		}
	case tz != "":
		colon := tz[0] == ':'
		if colon {
			tz = tz[1:]
		}
		if tz != "" && tz[0] == '/' {
			if z, err := loadLocation(tz, []string{""}); err == nil {
				localLoc = *z
				if tz == "/etc/localtime" {
					localLoc.name = "Local"
				} else {
					localLoc.name = tz
				}
				return
			}
		} else if tz != "" && tz != "UTC" {
			var u unknownTimezoneError
			if z, err := loadLocation(tz, platformZoneSources); err == nil {
				localLoc = *z
				return
			} else if !colon && errors.As(err, &u) {
				if initLocalFromTZString(tz) {
					return
				}
			}
		}
	}

	// Fall back to UTC.
	localLoc.name = "UTC"
}

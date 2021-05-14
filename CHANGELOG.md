# Changelog

## [Version 2.0.0 - May 14, 2021](https://github.com/vkatsuba/bellboy/releases/tag/2.0.0)
* [#1](https://github.com/vkatsuba/bellboy/pull/1) Upgrating ([manuel-rubio](https://github.com/manuel-rubio))
  * Add `rebar.lock` ensuring use always the same dependencies
  * Fix `application:get_env` (return always a string)
  * Start use [jsone](https://github.com/sile/jsone) instead of [jsx](https://github.com/talentdeficit/jsx)
  * Add tests
* Added GitHub Actions

## [Version 1.0.1 - Feb 20, 2021](https://github.com/vkatsuba/bellboy/releases/tag/1.0.1)
* Remove include file
* Remove `uuid` non use dependency
* Update description `*spp.src`

## [Version 1.0.0 - Feb 14, 2021](https://github.com/vkatsuba/bellboy/releases/tag/1.0.0)
* Removed `Makefile`
* Replaced `jiffy` to `jsx`
* Added `rebar3` to `.gitignore`
* Changed `bellboy` architecture from application to library
* Added `rebar3_hex`
* Added [SemVer](https://semver.org/)
* Added `hex` link
* Added license

## [Version 0.1.0 - Feb 14, 2021](https://github.com/vkatsuba/bellboy/releases/tag/0.1.0)
---------------------------
* Created base application
* Added support `nexmo`
* Added support `twilio`
* Added support `plivo`

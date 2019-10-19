# bellboy
Bellboy - is HTTP client library for send SMS by different services: Plivo, Twilio, Nexmo

## Contents
* [Goals](#goals)
* [Build & Run](#build--run)
* [Clean Project](#clean-project)
* [Install bellboy to project](#install-bellboy-to-project-rebar3)
* [Plivo](#plivo)
* [Twilio](#twilio)
* [Nexmo](#nexmo)
* [Support](#support)

# Goals
...

# Build & Run
```sh
$ git clone https://github.com/vkatsuba/bellboy.git
$ cd bellboy
$ make
```
# Clean Project
```sh
$ make clean
```
# Install bellboy to project: [Rebar3](https://www.rebar3.org/)
* Edit file **rebar.config**:
```
...
{deps, [
  ...
  {bellboy, {git, "git://github.com/vkatsuba/bellboy.git", {branch, "master"}}},
  ...
]}.
...
```
* Edit file ***.app.src**:
```
...
  {applications,
   [
    ...,
    bellboy,
    ...
   ]},
...
```

# Plivo
...

# Twilio
...

# Nexmo
...

# Support
v.katsuba.dev@gmail.com

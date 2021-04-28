Lince [![Build Status](https://travis-ci.org/arcalab/lince.svg?branch=master)](https://travis-ci.org/arcalab/lince)
========================

This prototype analyses hybrid programs using simulation based on symbolic evaluation (via [SageMath](http://www.sagemath.org/)) and estimation of approximation errors caused by small perturbations.

You can try it online using the link below, or install it on your computer (instructions below).
  * http://arcatools.org/#lince

Lince is developed in Scala, and uses ScalaJS to generate JavaScript.
It is developed as one of the sub-modules of ReoLive (https://github.com/ReoLanguage/ReoLive).


Local installation of Lince
==============
* Requirements:
    - SageMath (http://www.sagemath.org/)
    - SBT (https://www.scala-sbt.org)
    - Java runtime

* Clone the [ReoLive repository](https://github.com/ReoLanguage/ReoLive) (Lince is one of its submodules)

```
git clone git@github.com:ReoLanguage/ReoLive.git
cd ReoLive
```

* Pull the git submodules (which will include Lince):

```
git submodule update --init
```

* Use your favourite editor to edit the path to SageMath executables in:

```
global.properties
```

* Run the compilation script:

```
./compile.sh
```

* During development you can recompile faster using `sbt fastOptJS`.


How to run the framework
=====

* Start the server using `sbt`

```
sbt server/run
``` 

*  Open `localhost:9000` in a browser

```
open http://localhost:9000#lince
```

* Alternatively, open in a browser the local html file `site/lince.html` (or `site/index.html`)

```
open file/lince.html
```

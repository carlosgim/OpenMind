# OpenMind

**OpenMind** is an open source software based on a Deep Learning algorithm.  
This is a fast and simple (adaptable) algorithm for application in Data Science 
projects. This code was implemented in modular shape.

**If you'd like to contribute to OpenMind, be sure to review the [contribution
guidelines](CONTRIBUTING.md).**

**We use [GitHub issues](https://github.com/carlosgim/OpenMind/issues) for
tracking requests and bugs.

**Mailing List: https://groups.google.com/forum/#!forum/openmind_dl

## Requirements

 1. [cmake](https://cmake.org/)
 2. [gfortran](https://gcc.gnu.org/wiki/GFortranBinaries)
 3. [python 3.5](https://www.python.org/) - You can use 2.7 if you change the run_script.py file.

## Installation

```shell
./setup
```

Where you have the follow options:

0. Install (the binary go to /bin folder)
1. Run Test: This option run and example test (see examples).
2. Score data basis: This option just run the last ANN saved and allow to score a basis set.
3. Clean test folder: Clean the environment.
4. Debug: Run test and save some variables in temporal file.

## Run arbitrary basis set

After compile the code, you get the binary file OpenMind.exe. You can copy that file in whatever folder with the input file from example (test/example/*.inp)

Then you run in shell

```shell
OpenMind.exe
```

### Input file

```
**INPUT FILE NN_KERNEL
*KINDRUN
1
*READFILE
Basis_Train.csv
*SCORE FILE (IF KINDRUN>1)
Basis_Train.csv
*HIDDEN
20
*LAYER
1
*EPOCH
100
*LEARNING RATE (0-2)
1.70
*PERCENT TRAIN (0-100)
70
*PERCENT TEST (0-100)
30
*HEADER LINES
1
*SCREEN OUTPUT
1
*OUTPUT NAME
Basis_Train_scored.csv
*DEBUG
.TRUE.
```
Where

**KINDRUN**

have the follow options: 
 1. Train the Neural Network
 2. Add score to basis set
 3. Score one register
 4. Debug
 
**READFILE**

Name of the file for training the neural network.
 
 **SCORE FILE** (IF KINDRUN>1)
 
 Name of the file of basis set for add score
 
 **HIDDEN**
 
 Number of hidden neurons
 
**LAYER**

Number of layer (in construction...)

**EPOCH**

Epoch

**LEARNING RATE (0-2)**

Learning rate

**PERCENT TRAIN (0-100) and PERCENT TEST (0-100)**

How we split the basis set

**HEADER LINES**

Number of header lines in basis set

**SCREEN OUTPUT**

Screen output actualization

**OUTPUT NAME**

Name of the basis set scored

**DEBUG**

.TRUE. For debugging
.FALSE. Not debugging

## Contact

www.carlosagimenez.com

## References 

1. [Briandolhasky] (http://briandolhansky.com/blog/2014/10/30/artificial-neural-networks-matrix-form-part-5)
2. [Phil Brierley] (http://www.philbrierley.com)


# Ruby Minips (MIPS Simulator)

MIPS (Microprocessor without Interlocked Pipelined Stages) is a reduced instruction set computer (RISC) instruction set architecture (ISA) developed by MIPS Computer Systems, now MIPS Technologies, based in the United States. [[1]].
MIPS has 32 and 64-bit versions, this simulator was designed to work with the 32-bit version

## Registers

MIPS has thirty-two 32-bit registers. Register `$zero` is an immutable register and register `$ra` (number 31) a link register

## Instructions

Instructions are 32-bit words that can be splited as follows for decoding:

![alt](https://github.com/gabriel-murakami/ruby_minips/blob/main/instructions_format.png)

## Input

Input files are divided into `.text` containing executable binaries and `.data` containing global and static variables

## Usage

### #decode

Prints the instructions contained in the `.text` file

Ex:
```
$ ruby ruby_minips decode 01.soma
```
output:
```
addi $t0, $zero, 3
addi $t1, $zero, 4
add $s0, $t0, $t1
addi $v0, $zero, 1
add $a0, $zero, $s0
syscall
addi $v0, $zero, 10
syscall
```

### #run

Executes the instructions listed in `.text` using the `.data` variables if there are any

Ex:
```
$ ruby ruby_minips.rb run 02.hello
```
output:
```
Ola mundo!

Execution finished successfully
------------------------------
Instruction Count: 9 (R: 3, I: 6, J: 0)
IPS: 2858.9580686149934s
```
ps: IPS (Instructions per second)

[1]: https://en.wikipedia.org/wiki/MIPS_architecture

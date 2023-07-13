# Ruby Minips (MIPS Simulator)

MIPS (Microprocessor without Interlocked Pipelined Stages) is a reduced instruction set computer (RISC) instruction set architecture (ISA) developed by MIPS Computer Systems, now MIPS Technologies, based in the United States. [[1]].
MIPS has 32 and 64-bit versions, this simulator was designed to work with the 32-bit version

## Registers

MIPS has thirty-two 32-bit registers. Register `$zero` is an immutable register and register `$ra` (number 31) a link register

## Instructions

Instructions are 32-bit words that can be splited as follows for decoding:

![alt](https://github.com/gabriel-murakami/ruby_minips/blob/master/instructions_format.png)

## Input

Input files are divided into `.text` containing executable binaries and `.data` containing global and static variables

## Usage

### #decode

Prints the instructions contained in the `.text` file

Ex:
```
$ bundle exec ruby ruby_minips decode 01.soma
```
output:
```
0x00400000:       0x20080003       addi $t0, $zero, 3
0x00400004:       0x20090004       addi $t1, $zero, 4
0x00400008:       0x01098020       add $s0, $t0, $t1
0x0040000c:       0x20020001       addi $v0, $zero, 1
0x00400010:       0x00102020       add $a0, $zero, $s0
0x00400014:       0x0000000c       syscall
0x00400018:       0x2002000a       addi $v0, $zero, 10
0x0040001c:       0x0000000c       syscall
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
Instruction Count: 9 (R: 3, I: 6, J: 0, FR: 0, FI: 0)
IPS: 623.2643386635592s

Simulated execution times for:
------------------------------
Monocyle
  Cycles: 1616
  Frequency: 8.4672 MHz
  Estimated execution time: 0.00019
  IPC: 0.01
  MIPS: 0.05
Pipelined
  Cycles: 1620
  Frequency: 33.8688 MHz
  Estimated execution time: 0.00000
  IPC: 0.01
  MIPS: 0.19

Memory Information
------------------------------
Level  Hits       Misses     Total      Miss Rate
------ ---------- ---------- ---------- ----------
   RAM         16          0         16        0 %
```
ps: IPS (Instructions per second)

[1]: https://en.wikipedia.org/wiki/MIPS_architecture

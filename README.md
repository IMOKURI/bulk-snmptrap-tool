# snmptrapper

Tool that sends many SNMP traps for load test of SNMP manager  

[![Build Status](https://travis-ci.org/IMOKURI/snmptrapper.svg?branch=master)](https://travis-ci.org/IMOKURI/snmptrapper)  


## Description

Send SNMP traps by multi threads.  
Each thread is made by each section in configurations file.  
After specific time, each thread is stopped and output the number of sent trap.  


## Constraint

* General constraint
    + Support protocol: only UDP
    + Support address: only ipv4
    + Support SNMP version: v1, v2c
    + Support variable bindings: supported types are only string and integer.
    + Time ticks of trap: fixed number(12345)

* SNMP v2c constraint
    + Request id of trap: fixed number(12345)


## Configuration file format

* Specific information of each thread of sending traps.(SNMP v1)

|Option            |Value                              |Description                                                 |
|:-----------------|:----------------------------------|:-----------------------------------------------------------|
|snmp_version      |1                                  |SNMP version.                                               |
|snmp_community    |public                             |Community name for trap. (default: public)                  |
|agent_ip_address  |192.168.xxx.xxx                    |Source address of trap. (default: 127.0.0.1)                |
|enterprise_oid    |.1.3.6.1.4.1.xxxx                  |Enterprise id of trap.                                      |
|generic_trap      |6                                  |Generic trap of trap.                                       |
|specific_trap     |1001                               |Specific id of trap.                                        |
|varbind           |.1.3.6.1.4.1.xxxx.1 s Hello world. |LF separated list of oid, type and message. (#1) (optional) |


* Specific information of each thread of sending traps.(SNMP v2c)

|Option            |Value                       |Description                                                 |
|:-----------------|:---------------------------|:-----------------------------------------------------------|
|snmp_version      |2c                          |SNMP version.                                               |
|snmp_community    |public                      |Community name for trap. (default: public)                  |
|snmptrap_oid      |.1.3.6.1.4.1.xxxx.0.1001    |SNMP trap oid of trap.                                      |
|varbind           |.1.3.6.1.4.1.xxxx.1 i 12345 |LF separated list of oid, type and message. (#1) (optional) |

(#1) If you use multiple values, you need to insert a space in front of new line.  

* Following types are supported.

|Value  |Description    |
|:------|:--------------|
|s      |String         |
|i      |Integer        |


## Install

`runhaskell Setup configure`  
`runhaskell Setup build`  
`runhaskell Setup install`  


## Usage

```
Usage: snmptrapper --config CONFIGFILE --host HOSTNAME
                   [--port PORT] [--intval INTERVAL(microsec)] [--timer TIMER(sec)] [+RTS -N]

Available options:
  -h,--help                Show this help text
  --config CONFIGFILE      CONFIGFILE that is used for sending SNMP traps
  --host HOSTNAME          HOSTNAME that is sent SNMP traps
  --port PORT              PORT that is sent SNMP traps (default: 162)
  --intval INTERVAL        Transmission INTERVAL (microsecond (10^-6)) (default: 1000000)
  --timer TIMER            Transmission TIMER (second) (default: 10)
  +RTS -N                  You can use multi cores
```


## ToDo

* Add tests.


## Conversation

[![Join the chat at https://gitter.im/IMOKURI/snmptrapper](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/IMOKURI/snmptrapper?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)  


# ANOMLOG Unit Tests

## Overview
This directory contains comprehensive unit tests for the ANOMLOG.CBL logging subprogram using the ASSEQ.CBL assertion framework.

## Test Files

### TEST-ANOMLOG.CBL (TESTANOML)
Main test program that validates all ANOMLOG functionality:

**Test Cases:**
1. **TEST-ANOMALY-MESSAGE** - Tests ANOMALY message type formatting
2. **TEST-ERROR-MESSAGE** - Tests ERROR message type formatting
3. **TEST-WARNING-MESSAGE** - Tests WARNING message type formatting
4. **TEST-FATAL-MESSAGE** - Tests FATAL message type formatting
5. **TEST-ABEND-MESSAGE** - Tests ABEND message type formatting
6. **TEST-INFO-MESSAGE** - Tests INFO message type formatting
7. **TEST-HIGH-RETURN-CODE** - Tests maximum return code (9999)
8. **TEST-ZERO-RETURN-CODE** - Tests zero return code

**Coverage:**
- All 6 message types (ANOMALY, ERROR, WARNING, FATAL, ABEND, INFO)
- Return code handling (0, 4, 8, 12, 16, 999, 9999)
- Description truncation to fit 80-character log format
- Log file creation and write operations
- Parameter block structure validation

## JCL Files

### JCOMPTST.JCL
Compiles and links TEST-ANOMLOG.CBL with dependencies:
- Links ASSEQ assertion module
- Links ANOMLOG logging module
- Creates executable TESTANOML in API1.COBOL.LOAD

**Usage:**
```
Submit JCOMPTST.JCL to compile the test program
```

### JTESTANOML.JCL
Executes the TESTANOML test suite:
- Allocates temporary log file (&&TEMPLOG) for DDOUT2
- Runs all test cases
- Displays test results to SYSOUT

**Usage:**
```
Submit JTESTANOML.JCL to execute all unit tests
```

## Execution Sequence

1. **Compile ANOMLOG** (if not already done):
   ```
   Submit: companomlog.jcl
   ```

2. **Compile ASSEQ** (if not already done):
   ```
   Submit: JASSEQ (from TESTS/DEMOS)
   ```

3. **Compile Test Program**:
   ```
   Submit: JCOMPTST.JCL
   ```

4. **Run Tests**:
   ```
   Submit: JTESTANOML.JCL
   ```

## Expected Output

Successful test execution displays:
```
**************************************************
*** ANOMLOG UNIT TESTS ***
**************************************************
******** TEST-ANOMALY-MESSAGE *********
RUN>01 OK>01 KO>00
**************************************************
******** TEST-ERROR-MESSAGE *********
RUN>02 OK>02 KO>00
**************************************************
******** TEST-WARNING-MESSAGE *********
RUN>03 OK>03 KO>00
**************************************************
******** TEST-FATAL-MESSAGE *********
RUN>04 OK>04 KO>00
**************************************************
******** TEST-ABEND-MESSAGE *********
RUN>05 OK>05 KO>00
**************************************************
******** TEST-INFO-MESSAGE *********
RUN>06 OK>06 KO>00
**************************************************
******** TEST-HIGH-RETURN-CODE *********
RUN>07 OK>07 KO>00
**************************************************
******** TEST-ZERO-RETURN-CODE *********
RUN>08 OK>08 KO>00
**************************************************
*** ALL TESTS COMPLETED ***
TOTAL RUN: 08
TOTAL PASSED: 08
TOTAL FAILED: 00
**************************************************
```

## Test Framework

### ASSEQ.CBL - Assertion Tool
Located in: `TESTS/COPYBOOK/ASSEQ.CBL`

**Parameters:**
- TEST-CONTEXT: Tracks test counters (RUN, PASSES, FAILURES)
- TEST-NAME: Descriptive name for the test case
- EXPECTED: Expected numeric result (PIC S9(3)V99)
- ACTUAL: Actual numeric result (PIC S9(3)V99)

**Behavior:**
- Compares EXPECTED vs ACTUAL values
- Increments test counters
- Displays failure details if values don't match

### TESTCONT.CBL - Test Context Structure
Located in: `TESTS/COPYBOOK/TESTCONT.CBL`

```cobol
01 TEST-CONTEXT.
   02 TESTS-RUN PIC 9(2).
   02 PASSES    PIC 9(2).
   02 FAILURES  PIC 9(2).
```

## Test Strategy

The tests validate ANOMLOG by:
1. Calling ANOMLOG with various parameter combinations
2. Verifying successful completion (return code 0)
3. Confirming log file writes without errors
4. Testing all message type code paths
5. Testing boundary conditions (max/min return codes)

**Note:** These are functional tests that verify ANOMLOG executes without errors. Full validation would require reading back the log file to verify formatting, which is beyond the scope of this basic test suite.

## Troubleshooting

### Common Issues

**Compile Error: ANOMLOG copybook not found**
- Ensure ANOMLOG.CBL is in COPYBOOK directory
- Verify SYSLIB DD includes copybook dataset

**Link Error: ANOMLOG or ASSEQ not found**
- Compile ANOMLOG with companomlog.jcl
- Compile ASSEQ with JASSEQ

**Runtime Error: DDOUT2 not allocated**
- JCL must define DDOUT2 DD card
- JTESTANOML.JCL includes temporary allocation

**File Status 35: DDOUT2 not found**
- Check DD card in execution JCL
- Verify dataset name and allocation

## Dependencies

- **ANOMLOG.CBL** (COBOL/): Main logging program
- **ANOMLOG.CBL** (COPYBOOK/): Parameter block copybook
- **ASSEQ.CBL** (TESTS/COPYBOOK/): Assertion framework
- **TESTCONT.CBL** (TESTS/COPYBOOK/): Test context structure

## Future Enhancements

Potential test improvements:
1. Read back log file and validate format
2. Test description truncation boundaries
3. Test invalid message types
4. Test concurrent writes (multi-threading)
5. Test file error conditions
6. Performance testing with large volumes

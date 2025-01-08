# Counter Class in Smalltalk

This project demonstrates a simple `Counter` class implemented in Smalltalk, along with its corresponding test case.

## Class Definition: `Counter`

The `Counter` class is a straightforward implementation that allows incrementing and decrementing a count.

### Implementation

```smalltalk
Object subclass: #Counter
    instanceVariableNames: 'count'
    classVariableNames: ''
    package: 'MyCounter'.
```

### Description

The `Counter` class provides basic functionality to manage a numerical count.

### Methods

- **Accessing Methods:**

  - `count`: Returns the current count.

    ```smalltalk
    count
        ^count.
    ```

  - `count: aNumber`: Sets the count to `aNumber`.

    ```smalltalk
    count: aNumber
        count := aNumber.
    ```

## Test Case: `CounterTest`

To ensure the `Counter` class functions as expected, a test case is provided using the `TestCase` framework.

### Implementation

```smalltalk
TestCase subclass: #CounterTest
    instanceVariableNames: ''
    classVariableNames: ''
    package: 'MyCounter'.
```

### Test Method

- **`testCountIsSetAndRead`**: Verifies that the `count` can be set and retrieved correctly.

  ```smalltalk
  testCountIsSetAndRead
      | c |
      c := Counter new.
      c count: 7.
      self assert: c count equals: 7.
  ```

## Running the Tests

To execute the tests, use the built-in test runner in your Smalltalk environment.

1. **Open the Test Runner**: Navigate to the test runner tool in your Smalltalk environment.

2. **Locate `CounterTest`**: Find the `CounterTest` class in the list of test cases.

3. **Run the Test**: Execute the `testCountIsSetAndRead` method to verify the functionality of the `Counter` class.

If the test passes without errors, the `Counter` class is functioning as intended.

## Conclusion

This example provides a foundational understanding of creating a simple class in Smalltalk and writing corresponding test cases to ensure its functionality.

For more information on Smalltalk and its testing framework, refer to the [GNU Smalltalk User’s Guide: SUnit](https://www.gnu.org/software/smalltalk/manual/html_node/SUnit.html). 

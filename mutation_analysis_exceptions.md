# Mutation Analysis Exceptions

### Exception Type 1 - Unnecessary
The counters are used for recording the number/order for each statement, and for each block. This information is really just for readability and doesn't affect the correctness of the proof.

- ln 95	`resetCounters();`

### Exception Type 2 - Unconsequential Code
The return false statement for the visit methods of SimpleName, BooleanLiteral, NumberLiteral, and NullLiteral don't have any effect on functionality in the code. This is because these objects are always leaf objects and don't have any children, so whether you return true or false won't matter because there is nothing else to visit.

- ln 172 `return false;`
- ln 182 `return false;`
- ln 192 `return false;`
- ln 202 `return false;`

### Exception Type 3 - Our Java subset prevents creating a test case to kill the mutant
The surviving mutant for this statement is setting the equality check to `true`. Becasue any InfixExpression within our subset will follow one of the specified types in the if-else chain, we setting the last else if in the chain to true is redundant, and we can't create a test otherwise to kill the mutant.

- ln 326 `...} else if (operator.equals(InfixExpression.Operator.EQUALS)) {...`

### Exception Type 4 - Display test doesn't have a way to verify, and its result doesn't go up the chain and affect the result
This type of mutant removes the call to assertTrue that is added into the DynamicTest. What is actually used to check for pass/fail is the type that will be grabbed in the stack, while this test that the mutant removes is simply for enhanced readability on when looking at the type proof. I asked a TA about this one specifically and she confirmed my findings that there is not an easy way to kill this mutant and that it is unneccesary.

- ln 507 `() -> Assertions.assertNotEquals(TypeCheckTypes.ERROR, type));`
- ln 533 `() -> Assertions.assertNotEquals(TypeCheckTypes.ERROR, type));`
- ln 553 `DynamicTest test = DynamicTest.dynamicTest(displayName, () -> assertTrue(testValue));`
- ln 574 `DynamicTest test = DynamicTest.dynamicTest(displayName, () -> assertTrue(testValue));`
- ln 597 `DynamicTest test = DynamicTest.dynamicTest(displayName, () -> assertTrue(testValue));`
- ln 614 `DynamicTest.dynamicTest(displayName, () -> assertTrue(correctNumArgs));`
- ln 637 `DynamicTest test = DynamicTest.dynamicTest(displayName, () -> assertTrue(testValue));`
- ln 656 `DynamicTest test = DynamicTest.dynamicTest(displayName, () -> assertTrue(testValue));`
- ln 675 `test = DynamicTest.dynamicTest(displayName, () -> assertTrue(testValue));`
- ln 692 `return DynamicTest.dynamicTest("No obligations", () -> assertTrue(true));`

### Exception Type 5 - Print formatting doesn't matter
I included this conditional statement to make formatting look cleaner when the left side of two type compatibility tests is the same. Either way of printing is just fine, and it really is just for slightly enhanced readability in the type proof.

- ln 569 `String displayName = (leftType1.equals(leftType2))...`

package edu.byu.cs329.typechecker;

import static org.junit.Assert.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import edu.byu.cs329.utils.JavaSourceUtils;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Stream;

import org.eclipse.jdt.core.dom.ASTNode;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.DynamicContainer;
import org.junit.jupiter.api.DynamicNode;
import org.junit.jupiter.api.DynamicTest;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestFactory;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

@DisplayName("Tests for TypeCheckerBuilder")
public class TypeCheckBuilderTests {
  static final Logger log = LoggerFactory.getLogger(TypeCheckBuilderTests.class);
  private int numCompilationUnits;
  private int numTypeDeclarations;
  private int numMethods;
  private int numBlocks;
  private int numStatements;
  private int numSymbolTableLookups;
  private int numTypeCompatibleTests;
  private int numAllSafeTests;

  private boolean getTypeChecker(final String fileName, List<DynamicNode> tests) {
    ASTNode compilationUnit = JavaSourceUtils.getAstNodeFor(this, fileName);
    SymbolTableBuilder symbolTableBuilder = new SymbolTableBuilder();
    SymbolTable symbolTable = symbolTableBuilder.getSymbolTable(compilationUnit);
    TypeCheckBuilder typeCheckerBuilder = new TypeCheckBuilder();
    return typeCheckerBuilder.getTypeChecker(symbolTable, compilationUnit, tests);
  }
 
  private Stream<DynamicNode> testTypeSafety_Positive(String fileName) {
    List<DynamicNode> tests = new ArrayList<>();
    boolean isTypeSafe = getTypeChecker(fileName, tests);
    DynamicTest test = DynamicTest.dynamicTest("isTypeSafe", () -> assertTrue(isTypeSafe));
    tests.add((DynamicNode)test);
    return tests.stream();
  }

  private Stream<DynamicNode> testTypeSafety_Negative(String fileName, boolean showTree) {
    List<DynamicNode> tests = new ArrayList<>();
    boolean isTypeSafe = getTypeChecker(fileName, tests);

    if (showTree) {
      return tests.stream();
    }
    DynamicTest test = DynamicTest.dynamicTest("isNotTypeSafe", () -> assertFalse(isTypeSafe));
    return Arrays.asList((DynamicNode)test).stream();
  }

  private void testDynamicTestStructure(String fileName, int expMethods, int expBlocks, int expStatements,
      int expSymbolTableLookups, int expTypeCompatibleTests) {
    List<DynamicNode> tests = new ArrayList<>();
    getTypeChecker(fileName, tests);
    computeStructuralData(tests);

    assertEquals(1, this.numCompilationUnits);
    assertEquals(1, this.numTypeDeclarations);
    assertEquals(expMethods, this.numMethods);
    assertEquals(expBlocks, this.numBlocks);
    assertEquals(expStatements, this.numStatements);
    assertEquals(expSymbolTableLookups, this.numSymbolTableLookups);
    assertEquals(expTypeCompatibleTests, this.numTypeCompatibleTests);
    assertEquals(2 + expMethods + expBlocks, this.numAllSafeTests);
  }

  private void computeStructuralData(List<DynamicNode> tests) {
    this.numCompilationUnits = 0;
    this.numTypeDeclarations = 0;
    this.numMethods = 0;
    this.numBlocks = 0;
    this.numStatements = 0;
    this.numSymbolTableLookups = 0;
    this.numTypeCompatibleTests = 0;
    this.numAllSafeTests = 0;

    traverseDynamicTests(tests.stream());
  }

  // private boolean testNameFound(String regex, Stream<? extends DynamicNode> tests) {
  //   tests.anyMatch(node -> {
  //     if (node instanceof DynamicTest) {
  //       DynamicTest test = (DynamicTest) node;
  //       return test.getDisplayName().matches(regex);
  //     }
  //     DynamicContainer container = (DynamicContainer) node;
  //     return testFoundInTests(regex, container.getChildren());
  //   });
  // }

  private boolean countainerNameFound(String regex, Stream<? extends DynamicNode> tests) {
    return tests.anyMatch(node -> {
      if (node instanceof DynamicTest) {
        return false;
      }
      DynamicContainer container = (DynamicContainer) node;
      if (container.getDisplayName().matches(regex)) {
        return true;
      }
      return countainerNameFound(regex, container.getChildren());
    });
  }

  private void traverseDynamicTests(Stream<? extends DynamicNode> tests) {
    tests.forEach(node -> {
      if (node instanceof DynamicTest) {
        recordDataForTest((DynamicTest) node);
        return;
      }
      DynamicContainer container = (DynamicContainer) node;
      recordDataForContainer(container);
      traverseDynamicTests(container.getChildren());
    });
  }

  private void recordDataForTest(DynamicTest test) {
    var displayName = test.getDisplayName();
    if (displayName.matches("E\\((.*)\\) = (.*)")) {
      this.numSymbolTableLookups += 1;
    } else if (displayName.matches("(.*):=(.*)")) {
      this.numTypeCompatibleTests += 1;
    } else if (displayName.matches("(.*) = (.*)") || displayName.equals("No obligations")) {
      this.numAllSafeTests += 1;
    }
  }

  private void recordDataForContainer(DynamicContainer container) {
    var displayName = container.getDisplayName();
    if (displayName.matches("CompilationUnit (.*)")) {
      this.numCompilationUnits += 1;
    } else if (displayName.matches("class (.*)")) {
      this.numTypeDeclarations += 1;
    } else if (displayName.matches("method(.*)")) {
      this.numMethods += 1;
    } else if (displayName.matches("B\\d+(.*)")) {
      this.numBlocks += 1;
    } else if (displayName.matches("S\\d+(.*)")) {
      this.numStatements += 1;
    }
  }

  @TestFactory
  @Tag("TypeDeclaration")
  @DisplayName("Should prove type safe when given empty class")
  Stream<DynamicNode> should_proveTypeSafe_when_givenEmptyClass() {
    String fileName = "typeChecker/should_proveTypeSafe_when_givenEmptyClass.java";
    return testTypeSafety_Positive(fileName);
  }

  @Test
  @DisplayName("Should contain proper structure when given empty class")
  void should_containProperStructure_when_givenEmptyClass() {
    String fileName = "typeChecker/should_proveTypeSafe_when_givenEmptyClass.java";
    testDynamicTestStructure(fileName, 0, 0, 0, 0, 0);
  }

  @TestFactory
  @Tag("MethodDeclaration")
  @DisplayName("Should prove type safe when given empty method")
  Stream<DynamicNode> should_proveTypeSafe_when_givenEmptyMethod() {
    String fileName = "typeChecker/should_proveTypeSafe_when_givenEmptyMethod.java";
    return testTypeSafety_Positive(fileName);
  }

  @Test
  @DisplayName("Should contain proper structure when given empty method")
  void should_containProperStructure_when_givenEmptyMethod() {
    String fileName = "typeChecker/should_proveTypeSafe_when_givenEmptyMethod.java";
    testDynamicTestStructure(fileName, 1, 1, 0, 0, 0);
  }

  @TestFactory
  @Tag("Block")
  @DisplayName("Should prove type safe when given empty block")
  Stream<DynamicNode> should_proveTypeSafe_when_givenEmptyBlock() {
    String fileName = "typeChecker/should_proveTypeSafe_when_givenEmptyBlock.java";
    return testTypeSafety_Positive(fileName);
  }

  // HERE
  @Test
  @DisplayName("Should contain proper structure when given empty block")
  void should_containProperStructure_when_givenEmptyBlock() {
    String fileName = "typeChecker/should_proveTypeSafe_when_givenEmptyBlock.java";
    testDynamicTestStructure(fileName, 1, 1, 0, 0, 0);
  }

  @TestFactory
  @Tag("VariableDeclarationStatement")
  @DisplayName("Should prove type safe when given variable declarations no inits")
  Stream<DynamicNode> should_proveTypeSafe_when_givenVariableDeclrationsNoInits() {
    String fileName = "typeChecker/should_proveTypeSafe_when_givenVariableDeclrationsNoInits.java";
    return testTypeSafety_Positive(fileName);
  }

  @Test
  @DisplayName("Should contain proper structure when given variable declarations no inits")
  void should_containProperStructure_when_givenVariableDeclrationsNoInit() {
    String fileName = "typeChecker/should_proveTypeSafe_when_givenVariableDeclrationsNoInits.java";
    testDynamicTestStructure(fileName, 1, 1, 3, 3, 0);
  }

  @TestFactory
  @Tag("VariableDeclarationStatement")
  @DisplayName("Should prove type safe when given variable declarations with compatible inits")
  Stream<DynamicNode> should_proveTypeSafe_when_givenVariableDeclrationsWithCompatibleInits() {
    String fileName = "typeChecker/should_proveTypeSafe_when_givenVariableDeclrationsWithCompatibleInits.java";
    return testTypeSafety_Positive(fileName);
  }

  @Test
  @DisplayName("Should contain proper structure when given variable declarations with compatible inits")
  void should_containProperStructure_when_givenVariableDeclrationsWithCompatibleInits() {
    String fileName = "typeChecker/should_proveTypeSafe_when_givenVariableDeclrationsWithCompatibleInits.java";
    testDynamicTestStructure(fileName, 1, 1, 5, 10, 5);
  }

  @TestFactory
  @Tag("VariableDeclarationStatement")
  @DisplayName("Should not prove type safe when given bad inits")
  Stream<DynamicNode> should_NotProveTypeSafe_when_givenBadInits() {
    String fileName = "typeChecker/should_NotProveTypeSafe_when_givenBadInits.java";
    return testTypeSafety_Negative(fileName, false);
  }

  @Test
  @DisplayName("Should contain proper structure when given bad inits")
  void should_containProperStructure_when_givenBadInits() {
    String fileName = "typeChecker/should_NotProveTypeSafe_when_givenBadInits.java";
    testDynamicTestStructure(fileName, 1, 1, 4, 8, 4);
  }

  @TestFactory
  @Tag("Assignment")
  @DisplayName("Should prove type safe when given assignment with compatible operands")
  Stream<DynamicNode> should_proveTypeSafe_when_givenAssignmentWithCompatibleOperands() {
    String fileName = "typeChecker/should_proveTypeSafe_when_givenAssignmentWithCompatibleOperands.java";
    return testTypeSafety_Positive(fileName);
  }

  @Test
  @DisplayName("Should contain proper structure when given assignment with compatible operands")
  void should_containProperStructure_when_givenAssignmentWithCompatibleOperands() {
    String fileName = "typeChecker/should_proveTypeSafe_when_givenAssignmentWithCompatibleOperands.java";
    testDynamicTestStructure(fileName, 2, 2, 12, 18, 7);
  }

  @TestFactory
  @DisplayName("Should not prove type safe when given assignment where operands do not match")
  @Tag("Assignment")
  Stream<DynamicNode> should_NotProveTypeSafe_when_givenAssignmentWhereOperandsDoNotMatch() {
    String fileName = "typeChecker/should_NotProveTypeSafe_when_givenAssignmentWhereOperandsDoNotMatch.java";
    return testTypeSafety_Negative(fileName, false);
  }

  @Test
  @DisplayName("Should contain proper structure when given assignment where operands do not match")
  void should_containProperStructure_when_givenAssignmentWhereOperandsDoNotMatch() {
    String fileName = "typeChecker/should_NotProveTypeSafe_when_givenAssignmentWhereOperandsDoNotMatch.java";
    testDynamicTestStructure(fileName, 1, 1, 2, 3, 1);
  }

  @TestFactory
  @Tag("IfStatement")
  @DisplayName("Should prove type safe when given if statement with compatible types")
  Stream<DynamicNode> should_proveTypeSafe_when_givenIfStatementWithCompatibleTypes() {
    String fileName = "typeChecker/should_proveTypeSafe_when_givenIfStatementWithCompatibleTypes.java";
    return testTypeSafety_Positive(fileName);
  }

  @Test
  @DisplayName("Should contain proper structure when given if statement with compatible types")
  void should_containProperStructure_when_givenIfStatementWithCompatibleTypes() {
    String fileName = "typeChecker/should_proveTypeSafe_when_givenIfStatementWithCompatibleTypes.java";
    testDynamicTestStructure(fileName, 1, 4, 2, 2, 2);
  }

  @TestFactory
  @Tag("IfStatement")
  @DisplayName("Should not prove type safe when given if statement with type unsafe expression")
  Stream<DynamicNode> should_NotProveTypeSafe_when_givenIfStatementWithTypeUnsafeExpression() {
    String fileName = "typeChecker/should_NotProveTypeSafe_when_givenIfStatementWithTypeUnsafeExpression.java";
    return testTypeSafety_Negative(fileName, false);
  }

  @Test
  @DisplayName("Should contain proper structure when given if statement with type unsafe expression")
  void should_containProperStructure_when_givenIfStatementWithTypeUnsafeExpression() {
    String fileName = "typeChecker/should_NotProveTypeSafe_when_givenIfStatementWithTypeUnsafeExpression.java";
    testDynamicTestStructure(fileName, 1, 2, 1, 1, 1);
  }

  @TestFactory
  @Tag("IfStatement")
  @DisplayName("Should not prove type safe when given if statement with type unsafe then statement")
  Stream<DynamicNode> should_NotProveTypeSafe_when_givenIfStatementWithTypeUnsafeThenStatement() {
    String fileName = "typeChecker/should_NotProveTypeSafe_when_givenIfStatementWithTypeUnsafeThenStatement.java";
    return testTypeSafety_Negative(fileName, false); 
  }

  @Test
  @DisplayName("Should contain proper structure when given if statement with type unsafe then statement")
  void should_containProperStructure_when_givenIfStatementWithTypeUnsafeThenStatement() {
    String fileName = "typeChecker/should_NotProveTypeSafe_when_givenIfStatementWithTypeUnsafeThenStatement.java";
    testDynamicTestStructure(fileName, 1, 2, 2, 3, 2);
  }

  @TestFactory
  @Tag("IfStatement")
  @DisplayName("Should not prove type safe when given if statement with type unsafe else statement")
  Stream<DynamicNode> should_NotProveTypeSafe_when_givenIfStatementWithTypeUnsafeElseStatement() {
    String fileName = "typeChecker/should_NotProveTypeSafe_when_givenIfStatementWithTypeUnsafeElseStatement.java";
    return testTypeSafety_Negative(fileName, false);
  }

  @Test
  @DisplayName("Should contain proper structure when given if statement with type unsafe else statement")
  void should_containProperStructure_when_givenIfStatementWithTypeUnsafeElseStatement() {
    String fileName = "typeChecker/should_NotProveTypeSafe_when_givenIfStatementWithTypeUnsafeElseStatement.java";
    testDynamicTestStructure(fileName, 1, 3, 2, 3, 2);
  }

  @TestFactory
  @Tag("WhileStatement")
  @DisplayName("Should prove type safe when given while statement with compatible types")
  Stream<DynamicNode> should_proveTypeSafe_when_givenWhileStatementWithCompatibleTypes() {
    String fileName = "typeChecker/should_proveTypeSafe_when_givenWhileStatementWithCompatibleTypes.java";
    return testTypeSafety_Positive(fileName);
  }

  @Test
  @DisplayName("Should contain proper structure when given while statement with compatible types")
  void should_containProperStructure_when_givenWhileStatementWithCompatibleTypes() {
    String fileName = "typeChecker/should_proveTypeSafe_when_givenWhileStatementWithCompatibleTypes.java";
    testDynamicTestStructure(fileName, 1, 2, 1, 1, 1);
  }

  @TestFactory
  @Tag("WhileStatement")
  @DisplayName("Should not prove type safe when given while statement with type unsafe expression")
  Stream<DynamicNode> should_NotProveTypeSafe_when_givenWhileStatementWithTypeUnsafeExpression() {
    String fileName = "typeChecker/should_NotProveTypeSafe_when_givenWhileStatementWithTypeUnsafeExpression.java";
    return testTypeSafety_Negative(fileName, false);
  }

  @Test
  @DisplayName("Should contain proper structure when given while statement with type unsafe expression")
  void should_containProperStructure_when_givenWhileStatementWithTypeUnsafeExpression() {
    String fileName = "typeChecker/should_NotProveTypeSafe_when_givenWhileStatementWithTypeUnsafeExpression.java";
    testDynamicTestStructure(fileName, 1, 2, 1, 1, 1);
  }

  @TestFactory
  @Tag("WhileStatement")
  @DisplayName("Should not prove type safe when given while statement with type unsafe block")
  Stream<DynamicNode> should_NotProveTypeSafe_when_givenWhileStatementWithTypeUnsafeBlock() {
    String fileName = "typeChecker/should_NotProveTypeSafe_when_givenWhileStatementWithTypeUnsafeBlock.java";
    return testTypeSafety_Negative(fileName, false);
  }

  @Test
  @DisplayName("Should contain proper structure when given while statement with type unsafe block")
  void should_containProperStructure_when_givenWhileStatementWithTypeUnsafeBlock() {
    String fileName = "typeChecker/should_NotProveTypeSafe_when_givenWhileStatementWithTypeUnsafeBlock.java";
    testDynamicTestStructure(fileName, 1, 2, 2, 3, 2);
  }

  @TestFactory
  @Tag("ReturnStatement")
  @DisplayName("Should prove type safe when given return with compatible type")
  Stream<DynamicNode> should_proveTypeSafe_when_givenReturnWithCompatibleType() {
    String fileName = "typeChecker/should_proveTypeSafe_when_givenReturnWithCompatibleType.java";
    return testTypeSafety_Positive(fileName);
  }

  @Test
  @DisplayName("Should contain proper structure when given return with compatible type")
  void should_containProperStructure_when_givenReturnWithCompatibleType() {
    String fileName = "typeChecker/should_proveTypeSafe_when_givenReturnWithCompatibleType.java";
    testDynamicTestStructure(fileName, 4, 4, 3, 2, 3);
  }

  @TestFactory
  @Tag("ReturnStatement")
  @DisplayName("Should not prove type safe when given return with incompatible type")
  Stream<DynamicNode> should_NotProveTypeSafe_when_givenReturnWithIncompatibleType() {
    String fileName = "typeChecker/should_NotProveTypeSafe_when_givenReturnWithIncompatibleType.java";
    return testTypeSafety_Negative(fileName, false);
  }

  @Test
  @DisplayName("Should contain proper structure when given return with incompatible type")
  void should_containProperStructure_when_givenReturnWithIncompatibleType() {
    String fileName = "typeChecker/should_NotProveTypeSafe_when_givenReturnWithIncompatibleType.java";
    testDynamicTestStructure(fileName, 1, 1, 1, 1, 1);
  }

  @TestFactory
  @Tag("PrefixExpression")
  @DisplayName("Should prove type safe when given not prefix expression with compatible operand")
  Stream<DynamicNode> should_proveTypeSafe_when_givenNotPrefixExpressionWithCompatibleOperand() {
    String fileName = "typeChecker/should_proveTypeSafe_when_givenNotPrefixExpressionWithCompatibleOperand.java";
    return testTypeSafety_Positive(fileName);
  }

  @Test
  @DisplayName("Should contain proper structure when given not prefix expression with compatible operand")
  void should_containProperStructure_when_givenNotPrefixExpressionWithCompatibleOperand() {
    String fileName = "typeChecker/should_proveTypeSafe_when_givenNotPrefixExpressionWithCompatibleOperand.java";
    testDynamicTestStructure(fileName, 1, 1, 1, 2, 2);
  }

  @TestFactory
  @Tag("PrefixExpression")
  @DisplayName("Should not prove type safe when given not prefix expression where operand is not boolean")
  Stream<DynamicNode> should_NotProveTypeSafe_when_givenNotPrefixExpressionWhereOperandIsNotBoolean() {
    String fileName = "typeChecker/should_NotProveTypeSafe_when_givenNotPrefixExpressionWhereOperandIsNotBoolean.java";
    return testTypeSafety_Negative(fileName, false);
  }

  @Test
  @DisplayName("Should contain proper structure when given not prefix expression where operand is not boolean")
  void should_containProperStructure_when_givenNotPrefixExpressionWhereOperandIsNotBoolean() {
    String fileName = "typeChecker/should_NotProveTypeSafe_when_givenNotPrefixExpressionWhereOperandIsNotBoolean.java";
    testDynamicTestStructure(fileName, 1, 1, 2, 4, 3);
  }

  @TestFactory
  @Tag("InfixExpression")
  @DisplayName("Should prove type safe when given infix expression with compatible operands")
  Stream<DynamicNode> should_proveTypeSafe_when_givenInfixExpressionWithCompatibleOperands() {
    String fileName = "typeChecker/should_proveTypeSafe_when_givenInfixExpressionWithCompatibleOperands.java";
    return testTypeSafety_Positive(fileName);
  }

  @Test
  @DisplayName("Should contain proper structure when given infix expression with compatible operands")
  void should_containProperStructure_when_givenInfixExpressionWithCompatibleOperands() {
    String fileName = "typeChecker/should_proveTypeSafe_when_givenInfixExpressionWithCompatibleOperands.java";
    testDynamicTestStructure(fileName, 1, 1, 9, 23, 14);
  }

  @TestFactory
  @Tag("InfixExpression")
  @Tag("PlusMinusTimes")
  @DisplayName("Should not prove type safe when given infix expression with plus minus times where operand not int")
  Stream<DynamicNode> should_NotProveTypeSafe_when_givenInfixExpressionWithPlusMinusTimesWhereOperandNotInt() {
    String fileName = "typeChecker/should_NotProveTypeSafe_when_givenInfixExpressionWithPlusMinusTimesWhereOperandNotInt.java";
    return testTypeSafety_Negative(fileName, false);
  }

  @Test
  @DisplayName("Should contain proper structure when given infix expression with plus minus times where operand not int")
  void should_containProperStructure_when_givenInfixExpressionWithPlusMinusTimesWhereOperandNotInt() {
    String fileName = "typeChecker/should_NotProveTypeSafe_when_givenInfixExpressionWithPlusMinusTimesWhereOperandNotInt.java";
    testDynamicTestStructure(fileName, 1, 1, 1, 3, 2);
  }

  @TestFactory
  @Tag("InfixExpression")
  @Tag("AndOr")
  @DisplayName("Should not prove type safe when given infix expression with and or where operand not bool")
  Stream<DynamicNode> should_NotProveTypeSafe_when_givenInfixExpressionWithAndOrWhereOperandNotBool() {
    String fileName = "typeChecker/should_NotProveTypeSafe_when_givenInfixExpressionWithAndOrWhereOperandNotBool.java";
    return testTypeSafety_Negative(fileName, false);
  }

  @Test
  @DisplayName("Should contain proper structure when given infix expression with and or where operand not bool")
  void should_containProperStructure_when_givenInfixExpressionWithAndOrWhereOperandNotBool() {
    String fileName = "typeChecker/should_NotProveTypeSafe_when_givenInfixExpressionWithAndOrWhereOperandNotBool.java";
    testDynamicTestStructure(fileName, 1, 1, 1, 3, 2);
  }

  @TestFactory
  @Tag("InfixExpression")
  @Tag("LessThan")
  @DisplayName("Should not prove type safe when given infix expression with less than where operand not int")
  Stream<DynamicNode> should_NotProveTypeSafe_when_givenInfixExpressionWithLessThanWhereOperandNotInt() {
    String fileName = "typeChecker/should_NotProveTypeSafe_when_givenInfixExpressionWithLessThanWhereOperandNotInt.java";
    return testTypeSafety_Negative(fileName, false);
  }

  @Test
  @DisplayName("Should contain proper structure when given infix expression with less than where operand not int")
  void should_containProperStructure_when_givenInfixExpressionWithLessThanWhereOperandNotInt() {
    String fileName = "typeChecker/should_NotProveTypeSafe_when_givenInfixExpressionWithLessThanWhereOperandNotInt.java";
    testDynamicTestStructure(fileName, 1, 1, 1, 3, 2);
  }

  @TestFactory
  @Tag("InfixExpression")
  @Tag("Equals")
  @DisplayName("Should not prove type safe when given infix expression with equals where operand not object")
  Stream<DynamicNode> should_NotProveTypeSafe_when_givenInfixExpressionWithEqualsWhereOperandNotObject() {
    String fileName = "typeChecker/should_NotProveTypeSafe_when_givenInfixExpressionWithEqualsWhereOperandNotObject.java";
    return testTypeSafety_Negative(fileName, false);
  }

  @Test
  @DisplayName("Should contain proper structure when given infix expression with equals where operand not object")
  void should_containProperStructure_when_givenInfixExpressionWithEqualsWhereOperandNotObject() {
    String fileName = "typeChecker/should_NotProveTypeSafe_when_givenInfixExpressionWithEqualsWhereOperandNotObject.java";
    testDynamicTestStructure(fileName, 1, 1, 2, 5, 3);
  }

  @TestFactory
  @Tag("FieldAccess")
  @DisplayName("Should prove type safe when given field access with compatible type")
  Stream<DynamicNode> should_proveTypeSafe_when_givenFieldAccessWithCompatibleType() {
    String fileName = "typeChecker/should_proveTypeSafe_when_givenFieldAccessWithCompatibleType.java";
    return testTypeSafety_Positive(fileName);
  }

  @Test
  @DisplayName("Should contain proper structure when given field access with compatible type")
  void should_containProperStructure_when_givenFieldAccessWithCompatibleType() {
    String fileName = "typeChecker/should_proveTypeSafe_when_givenFieldAccessWithCompatibleType.java";
    testDynamicTestStructure(fileName, 1, 1, 3, 6, 3);
  }

  @TestFactory
  @Tag("FieldAccess")
  @DisplayName("Should not prove type safe when given field access with incompatible type")
  Stream<DynamicNode> should_NotProveTypeSafe_when_givenFieldAccessWithIncompatibleType() {
    String fileName = "typeChecker/should_NotProveTypeSafe_when_givenFieldAccessWithIncompatibleType.java";
    return testTypeSafety_Negative(fileName, false);
  }

  @Test
  @DisplayName("Should contain proper structure when given field access with incompatible type")
  void should_containProperStructure_when_givenFieldAccessWithIncompatibleType() {
    String fileName = "typeChecker/should_NotProveTypeSafe_when_givenFieldAccessWithIncompatibleType.java";
    testDynamicTestStructure(fileName, 1, 1, 1, 2, 1);
  }

  @TestFactory
  @Tag("QualifiedName")
  @DisplayName("Should prove type safe when given qualified name with compatible type")
  Stream<DynamicNode> should_proveTypeSafe_when_givenQualifiedNameWithCompatibleType() {
    String fileName = "typeChecker/should_proveTypeSafe_when_givenQualifiedNameWithCompatibleType.java";
    return testTypeSafety_Positive(fileName);
  }

  @Test
  @DisplayName("Should contain proper structure when given qualified name with compatible type")
  void should_containProperStructure_when_givenQualifiedNameWithCompatibleType() {
    String fileName = "typeChecker/should_proveTypeSafe_when_givenQualifiedNameWithCompatibleType.java";
    testDynamicTestStructure(fileName, 1, 1, 3, 6, 3);
  }

  @TestFactory
  @Tag("QualifiedName")
  @DisplayName("Should not prove type safe when given qualified name with incompatible type")
  Stream<DynamicNode> should_NotProveTypeSafe_when_givenQualifiedNameWithIncompatibleType() {
    String fileName = "typeChecker/should_NotProveTypeSafe_when_givenQualifiedNameWithIncompatibleType.java";
    return testTypeSafety_Negative(fileName, false);
  }

  @Test
  @DisplayName("Should contain proper structure when given qualified name with incompatible type")
  void should_containProperStructure_when_givenQualifiedNameWithIncompatibleType() {
    String fileName = "typeChecker/should_NotProveTypeSafe_when_givenQualifiedNameWithIncompatibleType.java";
    testDynamicTestStructure(fileName, 1, 1, 1, 2, 1);
  }

  @TestFactory
  @Tag("MethodInvocation")
  @DisplayName("Should prove type safe when given method invocation with compatible types")
  Stream<DynamicNode> should_proveTypeSafe_when_givenMethodInvocationWithCompatibleTypes() {
    String fileName = "typeChecker/should_proveTypeSafe_when_givenMethodInvocationWithCompatibleTypes.java";
    return testTypeSafety_Positive(fileName);
  }

  @Test
  @DisplayName("Should contain proper structure when given method invocation with compatible types")
  void should_containProperStructure_when_givenMethodInvocationWithCompatibleTypes() {
    String fileName = "typeChecker/should_proveTypeSafe_when_givenMethodInvocationWithCompatibleTypes.java";
    // incompatible type means that the compatibility test for 'a' in 'a(true)' is not run
    testDynamicTestStructure(fileName, 5, 5, 8, 19, 9);
  }

  @TestFactory
  @Tag("MethodInvocation")
  @DisplayName("Should not prove type safe when given method invocation with incorrect number of parameters")
  Stream<DynamicNode> should_NotProveTypeSafe_when_givenMethodInvocationWithIncorrectNumberOfParameters() {
    String fileName = "typeChecker/should_NotProveTypeSafe_when_givenMethodInvocationWithIncorrectNumberOfParameters.java";
    return testTypeSafety_Negative(fileName, false);
  }

  @Test
  @DisplayName("Should contain proper structure when given method invocation with incorrect number of parameters")
  void should_containProperStructure_when_givenMethodInvocationWithIncorrectNumberOfParameters() {
    String fileName = "typeChecker/should_NotProveTypeSafe_when_givenMethodInvocationWithIncorrectNumberOfParameters.java";
    // incorrect num parameters means that the compatibility test for '1' in 'a(1)'' is not run
    testDynamicTestStructure(fileName, 2, 2, 2, 3, 2);
  }

  @TestFactory
  @Tag("MethodInvocation")
  @DisplayName("Should not prove type safe when given method invocation with parameters with incompatable types")
  Stream<DynamicNode> should_NotProveTypeSafe_when_givenMethodInvocationWithParametersWithIncompatableTypes() {
    String fileName = "typeChecker/should_NotProveTypeSafe_when_givenMethodInvocationWithParametersWithIncompatableTypes.java";
    return testTypeSafety_Negative(fileName, false);
  }

  @Test
  @DisplayName("Should contain proper structure when given method invocation with parameters with incompatable types")
  void should_containProperStructure_when_givenMethodInvocationWithParametersWithIncompatableTypes() {
    String fileName = "typeChecker/should_NotProveTypeSafe_when_givenMethodInvocationWithParametersWithIncompatableTypes.java";
    testDynamicTestStructure(fileName, 2, 2, 0, 2, 1);
  }


  // NEW TESTS
  @TestFactory
  @Tag("ReturnStatement")
  @DisplayName("Should not prove type safe when given return with incompatible type null")
  Stream<DynamicNode> should_NotProveTypeSafe_when_givenReturnWithIncompatibleType_null() {
    String fileName = "typeChecker/mutation/should_NotProveTypeSafe_when_givenReturnWithIncompatibleType_null.java";
    return testTypeSafety_Negative(fileName, false);
  }

  @Test
  @Tag("")
  @DisplayName("Should exist container with return type in name when given container")
  void should_existContainerWithReturnTypeInName_whenGivenContainer() {
    String fileName = "typeChecker/mutation/should_existContainerWithReturnTypeInName_whenGivenContainer.java";
    List<DynamicNode> tests = new ArrayList<>();
    getTypeChecker(fileName, tests);
    String regex = "class(.*)" + TypeCheckTypes.VOID;
    assertTrue(countainerNameFound(regex, tests.stream()));
  }

   // Same test - but not running as Dynamic Test increases coverage
   @Test
   @Tag("VariableDeclarationStatement")
   @DisplayName("Should prove type safe and check five statements when given variable declarations with compatible inits")
   void should_proveTypeSafe_andCheckFiveStatements_when_givenVariableDeclrationsWithCompatibleInits() {
     String fileName = "typeChecker/should_proveTypeSafe_when_givenVariableDeclrationsWithCompatibleInits.java";
     List<DynamicNode> tests = new ArrayList<>();
     boolean isTypeSafe = getTypeChecker(fileName, tests);
     assertTrue(isTypeSafe);
   }

  // Same test - but not running as Dynamic Test increases coverage
  @Test
  @Tag("MethodInvocation")
  @DisplayName("Should prove type safe when given method invocation with compatible types not dynamic test")
  void should_ProveTypeSafe_when_givenMethodInvocationWithCompatibleTypes_notDynamicTest() {
    String fileName = "typeChecker/should_proveTypeSafe_when_givenMethodInvocationWithCompatibleTypes.java";
    List<DynamicNode> tests = new ArrayList<>();
    boolean isTypeSafe = getTypeChecker(fileName, tests);
    assertTrue(isTypeSafe);
  }
}

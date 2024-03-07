package edu.byu.cs329.typechecker;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import edu.byu.cs329.utils.JavaSourceUtils;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Stream;

import org.eclipse.jdt.core.dom.ASTNode;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.DynamicNode;
import org.junit.jupiter.api.DynamicTest;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.TestFactory;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

@DisplayName("Tests for TypeCheckerBuilder")
public class TypeCheckBuilderTests {
  static final Logger log = LoggerFactory.getLogger(TypeCheckBuilderTests.class);

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

  @TestFactory
  @Tag("TypeDeclaration")
  @DisplayName("Should prove type safe when given empty class")
  Stream<DynamicNode> should_proveTypeSafe_when_givenEmptyClass() {
    String fileName = "typeChecker/should_proveTypeSafe_when_givenEmptyClass.java";
    return testTypeSafety_Positive(fileName);
  }

  @TestFactory
  @Tag("MethodDeclaration")
  @DisplayName("Should prove type safe when given empty method")
  Stream<DynamicNode> should_proveTypeSafe_when_givenEmptyMethod() {
    String fileName = "typeChecker/should_proveTypeSafe_when_givenEmptyMethod.java";
    return testTypeSafety_Positive(fileName);
  }

  @TestFactory
  @Tag("Block")
  @DisplayName("Should prove type safe when given empty block")
  Stream<DynamicNode> should_proveTypeSafe_when_givenEmptyBlock() {
    String fileName = "typeChecker/should_proveTypeSafe_when_givenEmptyBlock.java";
    return testTypeSafety_Positive(fileName);
  }

  @TestFactory
  @Tag("VariableDeclarationStatement")
  @DisplayName("Should prove type safe when given variable declarations no inits")
  Stream<DynamicNode> should_proveTypeSafe_when_givenVariableDeclrationsNoInits() {
    String fileName = "typeChecker/should_proveTypeSafe_when_givenVariableDeclrationsNoInits.java";
    return testTypeSafety_Positive(fileName);
  }

  @TestFactory
  @Tag("VariableDeclarationStatement")
  @DisplayName("Should prove type safe when given variable declarations with compatible inits")
  Stream<DynamicNode> should_proveTypeSafe_when_givenVariableDeclrationsWithCompatibleInits() {
    String fileName = "typeChecker/should_proveTypeSafe_when_givenVariableDeclrationsWithCompatibleInits.java";
    return testTypeSafety_Positive(fileName);
  }

  @TestFactory
  @Tag("VariableDeclarationStatement")
  @DisplayName("Should not prove type safe when given bad inits")
  Stream<DynamicNode> should_NotProveTypeSafe_when_givenBadInits() {
    String fileName = "typeChecker/should_NotProveTypeSafe_when_givenBadInits.java";
    return testTypeSafety_Negative(fileName, false);
  }

  @TestFactory
  @Tag("Assignment")
  @DisplayName("Should prove type safe when given assignment with compatible operands")
  Stream<DynamicNode> should_proveTypeSafe_when_givenAssignmentWithCompatibleOperands() {
    String fileName = "typeChecker/should_proveTypeSafe_when_givenAssignmentWithCompatibleOperands.java";
    return testTypeSafety_Positive(fileName);
  }

  @TestFactory
  @DisplayName("Should not prove type safe when given assignment where operands do not match")
  @Tag("Assignment")
  Stream<DynamicNode> should_NotProveTypeSafe_when_givenAssignmentWhereOperandsDoNotMatch() {
    String fileName = "typeChecker/should_NotProveTypeSafe_when_givenAssignmentWhereOperandsDoNotMatch.java";
    return testTypeSafety_Negative(fileName, false);
  }

  @TestFactory
  @Tag("IfStatement")
  @DisplayName("Should prove type safe when given if statement with compatible types")
  Stream<DynamicNode> should_proveTypeSafe_when_givenIfStatementWithCompatibleTypes() {
    String fileName = "typeChecker/should_proveTypeSafe_when_givenIfStatementWithCompatibleTypes.java";
    return testTypeSafety_Positive(fileName);
  }

  @TestFactory
  @Tag("IfStatement")
  @DisplayName("Should not prove type safe when given if statement with type unsafe expression")
  Stream<DynamicNode> should_NotProveTypeSafe_when_givenIfStatementWithTypeUnsafeExpression() {
    String fileName = "typeChecker/should_NotProveTypeSafe_when_givenIfStatementWithTypeUnsafeExpression.java";
    return testTypeSafety_Negative(fileName, false);
  }

  @TestFactory
  @Tag("IfStatement")
  @DisplayName("Should not prove type safe when given if statement with type unsafe then statement")
  Stream<DynamicNode> should_NotProveTypeSafe_when_givenIfStatementWithTypeUnsafeThenStatement() {
    String fileName = "typeChecker/should_NotProveTypeSafe_when_givenIfStatementWithTypeUnsafeThenStatement.java";
    return testTypeSafety_Negative(fileName, false);
  }

  @TestFactory
  @Tag("IfStatement")
  @DisplayName("Should not prove type safe when given if statement with type unsafe else statement")
  Stream<DynamicNode> should_NotProveTypeSafe_when_givenIfStatementWithTypeUnsafeElseStatement() {
    String fileName = "typeChecker/should_NotProveTypeSafe_when_givenIfStatementWithTypeUnsafeElseStatement.java";
    return testTypeSafety_Negative(fileName, false);
  }

  @TestFactory
  @Tag("WhileStatement")
  @DisplayName("Should prove type safe when given while statement with compatible types")
  Stream<DynamicNode> should_proveTypeSafe_when_givenWhileStatementWithCompatibleTypes() {
    String fileName = "typeChecker/should_proveTypeSafe_when_givenWhileStatementWithCompatibleTypes.java";
    return testTypeSafety_Positive(fileName);
  }

  @TestFactory
  @Tag("WhileStatement")
  @DisplayName("Should not prove type safe when given while statement with type unsafe expression")
  Stream<DynamicNode> should_NotProveTypeSafe_when_givenWhileStatementWithTypeUnsafeExpression() {
    String fileName = "typeChecker/should_NotProveTypeSafe_when_givenWhileStatementWithTypeUnsafeExpression.java";
    return testTypeSafety_Negative(fileName, false);
  }

  @TestFactory
  @Tag("WhileStatement")
  @DisplayName("Should not prove type safe when given while statement with type unsafe block")
  Stream<DynamicNode> should_NotProveTypeSafe_when_givenWhileStatementWithTypeUnsafeBlock() {
    String fileName = "typeChecker/should_NotProveTypeSafe_when_givenWhileStatementWithTypeUnsafeBlock.java";
    return testTypeSafety_Negative(fileName, false);
  }

  @TestFactory
  @Tag("ReturnStatement")
  @DisplayName("Should prove type safe when given return with compatible type")
  Stream<DynamicNode> should_proveTypeSafe_when_givenReturnWithCompatibleType() {
    String fileName = "typeChecker/should_proveTypeSafe_when_givenReturnWithCompatibleType.java";
    return testTypeSafety_Positive(fileName);
  }

  @TestFactory
  @Tag("ReturnStatement")
  @DisplayName("Should not prove type safe when given return with incompatible type")
  Stream<DynamicNode> should_NotProveTypeSafe_when_givenReturnWithIncompatibleType() {
    String fileName = "typeChecker/should_NotProveTypeSafe_when_givenReturnWithIncompatibleType.java";
    return testTypeSafety_Negative(fileName, false);
  }

  @TestFactory
  @Tag("PrefixExpression")
  @DisplayName("Should prove type safe when given not prefix expression with compatible operand")
  Stream<DynamicNode> should_proveTypeSafe_when_givenNotPrefixExpressionWithCompatibleOperand() {
    String fileName = "typeChecker/should_proveTypeSafe_when_givenNotPrefixExpressionWithCompatibleOperand.java";
    return testTypeSafety_Positive(fileName);
  }

  @TestFactory
  @Tag("PrefixExpression")
  @DisplayName("Should not prove type safe when given not prefix expression where operand is not boolean")
  Stream<DynamicNode> should_NotProveTypeSafe_when_givenNotPrefixExpressionWhereOperandIsNotBoolean() {
    String fileName = "typeChecker/should_NotProveTypeSafe_when_givenNotPrefixExpressionWhereOperandIsNotBoolean.java";
    return testTypeSafety_Negative(fileName, false);
  }

  @TestFactory
  @Tag("InfixExpression")
  @DisplayName("Should prove type safe when given infix expression with compatible operands")
  Stream<DynamicNode> should_proveTypeSafe_when_givenInfixExpressionWithCompatibleOperands() {
    String fileName = "typeChecker/should_proveTypeSafe_when_givenInfixExpressionWithCompatibleOperands.java";
    return testTypeSafety_Positive(fileName);
  }

  @TestFactory
  @Tag("InfixExpression")
  @Tag("PlusMinusTimes")
  @DisplayName("Should not prove type safe when given infix expression with plus minus times where operand not int")
  Stream<DynamicNode> should_NotProveTypeSafe_when_givenInfixExpressionWithPlusMinusTimesWhereOperandNotInt() {
    String fileName = "typeChecker/should_NotProveTypeSafe_when_givenInfixExpressionWithPlusMinusTimesWhereOperandNotInt.java";
    return testTypeSafety_Negative(fileName, false);
  }

  @TestFactory
  @Tag("InfixExpression")
  @Tag("AndOr")
  @DisplayName("Should not prove type safe when given infix expression with and or where operand not bool")
  Stream<DynamicNode> should_NotProveTypeSafe_when_givenInfixExpressionWithAndOrWhereOperandNotBool() {
    String fileName = "typeChecker/should_NotProveTypeSafe_when_givenInfixExpressionWithAndOrWhereOperandNotBool.java";
    return testTypeSafety_Negative(fileName, false);
  }

  @TestFactory
  @Tag("InfixExpression")
  @Tag("LessThan")
  @DisplayName("Should not prove type safe when given infix expression with less than where operand not int")
  Stream<DynamicNode> should_NotProveTypeSafe_when_givenInfixExpressionWithLessThanWhereOperandNotInt() {
    String fileName = "typeChecker/should_NotProveTypeSafe_when_givenInfixExpressionWithLessThanWhereOperandNotInt.java";
    return testTypeSafety_Negative(fileName, false);
  }

  @TestFactory
  @Tag("InfixExpression")
  @Tag("Equals")
  @DisplayName("Should not prove type safe when given infix expression with equals where operand not object")
  Stream<DynamicNode> should_NotProveTypeSafe_when_givenInfixExpressionWithEqualsWhereOperandNotObject() {
    String fileName = "typeChecker/should_NotProveTypeSafe_when_givenInfixExpressionWithEqualsWhereOperandNotObject.java";
    return testTypeSafety_Negative(fileName, false);
  }

  @TestFactory
  @Tag("FieldAccess")
  @DisplayName("Should prove type safe when given field access with compatible type")
  Stream<DynamicNode> should_proveTypeSafe_when_givenFieldAccessWithCompatibleType() {
    String fileName = "typeChecker/should_proveTypeSafe_when_givenFieldAccessWithCompatibleType.java";
    return testTypeSafety_Positive(fileName);
  }

  @TestFactory
  @Tag("FieldAccess")
  @DisplayName("Should not prove type safe when given field access with incompatible type")
  Stream<DynamicNode> should_NotProveTypeSafe_when_givenFieldAccessWithIncompatibleType() {
    String fileName = "typeChecker/should_NotProveTypeSafe_when_givenFieldAccessWithIncompatibleType.java";
    return testTypeSafety_Negative(fileName, false);
  }

  @TestFactory
  @Tag("QualifiedName")
  @DisplayName("Should prove type safe when given qualified name with compatible type")
  Stream<DynamicNode> should_proveTypeSafe_when_givenQualifiedNameWithCompatibleType() {
    String fileName = "typeChecker/should_proveTypeSafe_when_givenQualifiedNameWithCompatibleType.java";
    return testTypeSafety_Positive(fileName);
  }

  @TestFactory
  @Tag("QualifiedName")
  @DisplayName("Should not prove type safe when given qualified name with incompatible type")
  Stream<DynamicNode> should_NotProveTypeSafe_when_givenQualifiedNameWithIncompatibleType() {
    String fileName = "typeChecker/should_NotProveTypeSafe_when_givenQualifiedNameWithIncompatibleType.java";
    return testTypeSafety_Negative(fileName, false);
  }

  @TestFactory
  @Tag("MethodInvocation")
  @DisplayName("Should prove type safe when given method invocation with compatible types")
  Stream<DynamicNode> should_proveTypeSafe_when_givenMethodInvocationWithCompatibleTypes() {
    String fileName = "typeChecker/should_proveTypeSafe_when_givenMethodInvocationWithCompatibleTypes.java";
    return testTypeSafety_Positive(fileName);
  }

  @TestFactory
  @Tag("MethodInvocation")
  @DisplayName("Should not prove type safe when given method invocation with incorrect number of parameters")
  Stream<DynamicNode> should_NotProveTypeSafe_when_givenMethodInvocationWithIncorrectNumberOfParameters() {
    String fileName = "typeChecker/should_NotProveTypeSafe_when_givenMethodInvocationWithIncorrectNumberOfParameters.java";
    return testTypeSafety_Negative(fileName, false);
  }

  @TestFactory
  @Tag("MethodInvocation")
  @DisplayName("Should not prove type safe when given method invocation with parameters with incompatable types")
  Stream<DynamicNode> should_NotProveTypeSafe_when_givenMethodInvocationWithParametersWithIncompatableTypes() {
    String fileName = "typeChecker/should_NotProveTypeSafe_when_givenMethodInvocationWithParametersWithIncompatableTypes.java";
    return testTypeSafety_Negative(fileName, false);
  }
}

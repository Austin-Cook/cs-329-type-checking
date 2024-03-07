package edu.byu.cs329.typechecker;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import edu.byu.cs329.utils.JavaSourceUtils;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Stream;

import org.eclipse.jdt.core.dom.ASTNode;
import org.eclipse.jdt.core.dom.PrefixExpression;
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
 
  @TestFactory
  @Tag("TypeDeclaration")
  @DisplayName("Should prove type safe when given empty class")
  Stream<DynamicNode> should_proveTypeSafe_when_givenEmptyClass() {
    String fileName = "typeChecker/should_proveTypeSafe_when_givenEmptyClass.java";
    List<DynamicNode> tests = new ArrayList<>();
    boolean isTypeSafe = getTypeChecker(fileName, tests);
    DynamicTest test = DynamicTest.dynamicTest("isTypeSafe", () -> assertTrue(isTypeSafe));
    tests.add((DynamicNode)test);
    return tests.stream();
  }

  @TestFactory
  @Tag("MethodDeclaration")
  @DisplayName("Should prove type safe when given empty method")
  Stream<DynamicNode> should_proveTypeSafe_when_givenEmptyMethod() {
    String fileName = "typeChecker/should_proveTypeSafe_when_givenEmptyMethod.java";
    List<DynamicNode> tests = new ArrayList<>();
    boolean isTypeSafe = getTypeChecker(fileName, tests);
    DynamicTest test = DynamicTest.dynamicTest("isTypeSafe", () -> assertTrue(isTypeSafe));
    tests.add((DynamicNode)test);
    return tests.stream();
  }

  @TestFactory
  @Tag("Block")
  @DisplayName("Should prove type safe when given empty block")
  Stream<DynamicNode> should_proveTypeSafe_when_givenEmptyBlock() {
    String fileName = "typeChecker/should_proveTypeSafe_when_givenEmptyBlock.java";
    List<DynamicNode> tests = new ArrayList<>();
    boolean isTypeSafe = getTypeChecker(fileName, tests);
    DynamicTest test = DynamicTest.dynamicTest("isTypeSafe", () -> assertTrue(isTypeSafe));
    tests.add((DynamicNode)test);
    return tests.stream();
  }

  @TestFactory
  @Tag("VariableDeclarationStatement")
  @DisplayName("Should prove type safe when given variable declarations no inits")
  Stream<DynamicNode> should_proveTypeSafe_when_givenVariableDeclrationsNoInits() {
    String fileName = "typeChecker/should_proveTypeSafe_when_givenVariableDeclrationsNoInits.java";
    List<DynamicNode> tests = new ArrayList<>();
    boolean isTypeSafe = getTypeChecker(fileName, tests);
    DynamicTest test = DynamicTest.dynamicTest("isTypeSafe", () -> assertTrue(isTypeSafe));
    tests.add((DynamicNode)test);
    return tests.stream();
  }

  @TestFactory
  @Tag("VariableDeclarationStatement")
  @DisplayName("Should prove type safe when given variable declarations with compatible inits")
  Stream<DynamicNode> should_proveTypeSafe_when_givenVariableDeclrationsWithCompatibleInits() {
    String fileName = "typeChecker/should_proveTypeSafe_when_givenVariableDeclrationsWithCompatibleInits.java";
    List<DynamicNode> tests = new ArrayList<>();
    boolean isTypeSafe = getTypeChecker(fileName, tests);
    DynamicTest test = DynamicTest.dynamicTest("isTypeSafe", () -> assertTrue(isTypeSafe));
    tests.add((DynamicNode)test);
    return tests.stream();
  }

  @TestFactory
  @Tag("VariableDeclarationStatement")
  @DisplayName("Should not prove type safe when given bad inits")
  Stream<DynamicNode> should_NotProveTypeSafe_when_givenBadInits() {
    String fileName = "typeChecker/should_NotProveTypeSafe_when_givenBadInits.java";
    List<DynamicNode> tests = new ArrayList<>();
    boolean isTypeSafe = getTypeChecker(fileName, tests);

    // Toggle as desired
    // 
    // Option 1: mvn exec:java shows the details of the typeproof for visual inspection
    // return tests.stream();
    //
    // Option 2: test only isNotTypeSafe and show no details
    DynamicTest test = DynamicTest.dynamicTest("isNotTypeSafe", () -> assertFalse(isTypeSafe));
    return Arrays.asList((DynamicNode)test).stream();
  }

  // Expression Statement for Assignment
  @TestFactory
  @Tag("Assignment")
  @DisplayName("Should prove type safe when given assignment with compatible operands")
  Stream<DynamicNode> should_proveTypeSafe_when_givenAssignmentWithCompatibleOperands() {
    String fileName = "typeChecker/should_proveTypeSafe_when_givenAssignmentWithCompatibleOperands.java";
    List<DynamicNode> tests = new ArrayList<>();
    boolean isTypeSafe = getTypeChecker(fileName, tests);
    DynamicTest test = DynamicTest.dynamicTest("isTypeSafe", () -> assertTrue(isTypeSafe));
    tests.add((DynamicNode)test);
    return tests.stream();
  }

  @TestFactory
  @DisplayName("Should not prove type safe when given assignment where operands do not match")
  @Tag("Assignment")
  Stream<DynamicNode> should_NotProveTypeSafe_when_givenAssignmentWhereOperandsDoNotMatch() {
    String fileName = "typeChecker/should_NotProveTypeSafe_when_givenAssignmentWhereOperandsDoNotMatch.java";
    List<DynamicNode> tests = new ArrayList<>();
    boolean isTypeSafe = getTypeChecker(fileName, tests);

    // Toggle as desired
    // 
    // Option 1: mvn exec:java shows the details of the typeproof for visual inspection
    // return tests.stream();
    //
    // Option 2: test only isNotTypeSafe and show no details
    DynamicTest test = DynamicTest.dynamicTest("isNotTypeSafe", () -> assertFalse(isTypeSafe));
    return Arrays.asList((DynamicNode)test).stream();
  }

  // IfStatement
  @TestFactory
  @Tag("IfStatement")
  @DisplayName("Should prove type safe when given if statement with compatible types")
  Stream<DynamicNode> should_proveTypeSafe_when_givenIfStatementWithCompatibleTypes() {
    String fileName = "typeChecker/should_proveTypeSafe_when_givenIfStatementWithCompatibleTypes.java";
    List<DynamicNode> tests = new ArrayList<>();
    boolean isTypeSafe = getTypeChecker(fileName, tests);
    DynamicTest test = DynamicTest.dynamicTest("isTypeSafe", () -> assertTrue(isTypeSafe));
    tests.add((DynamicNode)test);
    return tests.stream();
  }

  @TestFactory
  @Tag("IfStatement")
  @DisplayName("Should not prove type safe when given if statement with type unsafe expression")
  Stream<DynamicNode> should_NotProveTypeSafe_when_givenIfStatementWithTypeUnsafeExpression() {
    String fileName = "typeChecker/should_NotProveTypeSafe_when_givenIfStatementWithTypeUnsafeExpression.java";
    List<DynamicNode> tests = new ArrayList<>();
    boolean isTypeSafe = getTypeChecker(fileName, tests);

    // Toggle as desired
    // 
    // Option 1: mvn exec:java shows the details of the typeproof for visual inspection
    // return tests.stream();
    //
    // Option 2: test only isNotTypeSafe and show no details
    DynamicTest test = DynamicTest.dynamicTest("isNotTypeSafe", () -> assertFalse(isTypeSafe));
    return Arrays.asList((DynamicNode)test).stream();
  }

  @TestFactory
  @Tag("IfStatement")
  @DisplayName("Should not prove type safe when given if statement with type unsafe then statement")
  Stream<DynamicNode> should_NotProveTypeSafe_when_givenIfStatementWithTypeUnsafeThenStatement() {
    String fileName = "typeChecker/should_NotProveTypeSafe_when_givenIfStatementWithTypeUnsafeThenStatement.java";
    List<DynamicNode> tests = new ArrayList<>();
    boolean isTypeSafe = getTypeChecker(fileName, tests);

    // Toggle as desired
    // 
    // Option 1: mvn exec:java shows the details of the typeproof for visual inspection
    // return tests.stream();
    //
    // Option 2: test only isNotTypeSafe and show no details
    DynamicTest test = DynamicTest.dynamicTest("isNotTypeSafe", () -> assertFalse(isTypeSafe));
    return Arrays.asList((DynamicNode)test).stream();
  }

  @TestFactory
  @Tag("IfStatement")
  @DisplayName("Should not prove type safe when given if statement with type unsafe else statement")
  Stream<DynamicNode> should_NotProveTypeSafe_when_givenIfStatementWithTypeUnsafeElseStatement() {
    String fileName = "typeChecker/should_NotProveTypeSafe_when_givenIfStatementWithTypeUnsafeElseStatement.java";
    List<DynamicNode> tests = new ArrayList<>();
    boolean isTypeSafe = getTypeChecker(fileName, tests);

    // Toggle as desired
    // 
    // Option 1: mvn exec:java shows the details of the typeproof for visual inspection
    // return tests.stream();
    //
    // Option 2: test only isNotTypeSafe and show no details
    DynamicTest test = DynamicTest.dynamicTest("isNotTypeSafe", () -> assertFalse(isTypeSafe));
    return Arrays.asList((DynamicNode)test).stream();
  }

  // WhileStatemet

  
  // ReturnStatement
  @TestFactory
  @Tag("ReturnStatement")
  @DisplayName("Should prove type safe when given return with compatible type")
  Stream<DynamicNode> should_proveTypeSafe_when_givenReturnWithCompatibleType() {
    String fileName = "typeChecker/should_proveTypeSafe_when_givenReturnWithCompatibleType.java";
    List<DynamicNode> tests = new ArrayList<>();
    boolean isTypeSafe = getTypeChecker(fileName, tests);
    DynamicTest test = DynamicTest.dynamicTest("isTypeSafe", () -> assertTrue(isTypeSafe));
    tests.add((DynamicNode)test);
    return tests.stream();
  }

  @TestFactory
  @Tag("ReturnStatement")
  @DisplayName("Should not prove type safe when given return with incompatible type")
  Stream<DynamicNode> should_NotProveTypeSafe_when_givenReturnWithIncompatibleType() {
    String fileName = "typeChecker/should_NotProveTypeSafe_when_givenReturnWithIncompatibleType.java";
    List<DynamicNode> tests = new ArrayList<>();
    boolean isTypeSafe = getTypeChecker(fileName, tests);

    // Toggle as desired
    // 
    // Option 1: mvn exec:java shows the details of the typeproof for visual inspection
    // return tests.stream();
    //
    // Option 2: test only isNotTypeSafe and show no details
    DynamicTest test = DynamicTest.dynamicTest("isNotTypeSafe", () -> assertFalse(isTypeSafe));
    return Arrays.asList((DynamicNode)test).stream();
  }

  // PrefixExpression
  @TestFactory
  @Tag("PrefixExpression")
  @DisplayName("Should prove type safe when given not prefix expression with compatible operand")
  Stream<DynamicNode> should_proveTypeSafe_when_givenNotPrefixExpressionWithCompatibleOperand() {
    String fileName = "typeChecker/should_proveTypeSafe_when_givenNotPrefixExpressionWithCompatibleOperand.java";
    List<DynamicNode> tests = new ArrayList<>();
    boolean isTypeSafe = getTypeChecker(fileName, tests);
    DynamicTest test = DynamicTest.dynamicTest("isTypeSafe", () -> assertTrue(isTypeSafe));
    tests.add((DynamicNode)test);
    return tests.stream();
  }

  @TestFactory
  @Tag("PrefixExpression")
  @DisplayName("Should not prove type safe when given not prefix expression where operand is not boolean")
  Stream<DynamicNode> should_NotProveTypeSafe_when_givenNotPrefixExpressionWhereOperandIsNotBoolean() {
    String fileName = "typeChecker/should_NotProveTypeSafe_when_givenNotPrefixExpressionWhereOperandIsNotBoolean.java";
    List<DynamicNode> tests = new ArrayList<>();
    boolean isTypeSafe = getTypeChecker(fileName, tests);

    // Toggle as desired
    // 
    // Option 1: mvn exec:java shows the details of the typeproof for visual inspection
    // return tests.stream();
    //
    // Option 2: test only isNotTypeSafe and show no details
    DynamicTest test = DynamicTest.dynamicTest("isNotTypeSafe", () -> assertFalse(isTypeSafe));
    return Arrays.asList((DynamicNode)test).stream();
  }

  // InfixExpression
  // NOTE:
  // int for +, *, and -
  // boolean for &&, ||, <, and ==
  @TestFactory
  @Tag("InfixExpression")
  @DisplayName("Should prove type safe when given infix expression with compatible operands")
  Stream<DynamicNode> should_proveTypeSafe_when_givenInfixExpressionWithCompatibleOperands() {
    String fileName = "typeChecker/should_proveTypeSafe_when_givenInfixExpressionWithCompatibleOperands.java";
    List<DynamicNode> tests = new ArrayList<>();
    boolean isTypeSafe = getTypeChecker(fileName, tests);
    DynamicTest test = DynamicTest.dynamicTest("isTypeSafe", () -> assertTrue(isTypeSafe));
    tests.add((DynamicNode)test);
    return tests.stream();
  }

  @TestFactory
  @Tag("InfixExpression")
  @Tag("PlusMinusTimes")
  @DisplayName("Should not prove type safe when given infix expression with plus minus times where operand not int")
  Stream<DynamicNode> should_NotProveTypeSafe_when_givenInfixExpressionWithPlusMinusTimesWhereOperandNotInt() {
    String fileName = "typeChecker/should_NotProveTypeSafe_when_givenInfixExpressionWithPlusMinusTimesWhereOperandNotInt.java";
    List<DynamicNode> tests = new ArrayList<>();
    boolean isTypeSafe = getTypeChecker(fileName, tests);

    // Toggle as desired
    // 
    // Option 1: mvn exec:java shows the details of the typeproof for visual inspection
    // return tests.stream();
    //
    // Option 2: test only isNotTypeSafe and show no details
    DynamicTest test = DynamicTest.dynamicTest("isNotTypeSafe", () -> assertFalse(isTypeSafe));
    return Arrays.asList((DynamicNode)test).stream();
  }

  @TestFactory
  @Tag("InfixExpression")
  @Tag("AndOr")
  @DisplayName("Should not prove type safe when given infix expression with and or where operand not bool")
  Stream<DynamicNode> should_NotProveTypeSafe_when_givenInfixExpressionWithAndOrWhereOperandNotBool() {
    String fileName = "typeChecker/should_NotProveTypeSafe_when_givenInfixExpressionWithAndOrWhereOperandNotBool.java";
    List<DynamicNode> tests = new ArrayList<>();
    boolean isTypeSafe = getTypeChecker(fileName, tests);

    // Toggle as desired
    // 
    // Option 1: mvn exec:java shows the details of the typeproof for visual inspection
    // return tests.stream();
    //
    // Option 2: test only isNotTypeSafe and show no details
    DynamicTest test = DynamicTest.dynamicTest("isNotTypeSafe", () -> assertFalse(isTypeSafe));
    return Arrays.asList((DynamicNode)test).stream();
  }

  @TestFactory
  @Tag("InfixExpression")
  @Tag("LessThan")
  @DisplayName("Should not prove type safe when given infix expression with less than where operand not int")
  Stream<DynamicNode> should_NotProveTypeSafe_when_givenInfixExpressionWithLessThanWhereOperandNotInt() {
    String fileName = "typeChecker/should_NotProveTypeSafe_when_givenInfixExpressionWithLessThanWhereOperandNotInt.java";
    List<DynamicNode> tests = new ArrayList<>();
    boolean isTypeSafe = getTypeChecker(fileName, tests);

    // Toggle as desired
    // 
    // Option 1: mvn exec:java shows the details of the typeproof for visual inspection
    // return tests.stream();
    //
    // Option 2: test only isNotTypeSafe and show no details
    DynamicTest test = DynamicTest.dynamicTest("isNotTypeSafe", () -> assertFalse(isTypeSafe));
    return Arrays.asList((DynamicNode)test).stream();
  }

  @TestFactory
  @Tag("InfixExpression")
  @Tag("Equals")
  @DisplayName("Should not prove type safe when given infix expression with equals where operand not object")
  Stream<DynamicNode> should_NotProveTypeSafe_when_givenInfixExpressionWithEqualsWhereOperandNotObject() {
    String fileName = "typeChecker/should_NotProveTypeSafe_when_givenInfixExpressionWithEqualsWhereOperandNotObject.java";
    List<DynamicNode> tests = new ArrayList<>();
    boolean isTypeSafe = getTypeChecker(fileName, tests);

    // Toggle as desired
    // 
    // Option 1: mvn exec:java shows the details of the typeproof for visual inspection
    // return tests.stream();
    //
    // Option 2: test only isNotTypeSafe and show no details
    DynamicTest test = DynamicTest.dynamicTest("isNotTypeSafe", () -> assertFalse(isTypeSafe));
    return Arrays.asList((DynamicNode)test).stream();
  }



  // FieldAccess


  // QualifiedName


  // MethodInvocation

}

package typeChecker;

public class C {
  static int a;
  static boolean b;
  static C c;

  void m() {
    C.a = 1;
    C.b = true;
    C d = C.c;
  }
}

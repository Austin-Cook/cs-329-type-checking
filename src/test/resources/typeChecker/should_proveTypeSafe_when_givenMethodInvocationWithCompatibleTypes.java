package typeChecker;

public class C {
  int a(int a, boolean b) {
    return 1;
  }

  void b() {}

  static int c(int a, boolean b) {
    return 1;
  }

  void m() {
    int i = a(1, true);
    b();
    i = C.c(2, false);
  }
}
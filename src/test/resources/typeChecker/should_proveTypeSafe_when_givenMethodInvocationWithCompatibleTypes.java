package typeChecker;

public class C {
  int a(int a, boolean b, C c) {
    return 1;
  }

  void b() {}

  void d() {
    return;
  }

  static int c(int a, boolean b, C c) {
    return 1;
  }

  void m() {
    int x = 3;
    boolean y = true;
    C c;
    int i = a(1, true, c);
    b();
    d();
    i = C.c(2, false, c);
    C.c(2, false, c); // DELETEME
  }
}
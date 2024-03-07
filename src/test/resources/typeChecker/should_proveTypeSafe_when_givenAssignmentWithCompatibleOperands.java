package typeChecker;

public class C {
  int n() {
    return 1;
  }

  void m() {
    int a;
    a = 20;
    Integer b;
    b = null;
    C c;
    c = null;
    C d;
    d = c;
    boolean e;
    e = true;
    a = n();
  }
}

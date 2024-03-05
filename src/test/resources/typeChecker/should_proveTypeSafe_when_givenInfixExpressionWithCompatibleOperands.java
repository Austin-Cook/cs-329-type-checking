package typeChecker;

public class C {
  void m() {
    int i = 1 + 2;
    i = 1 - 2;
    i = 1 * 2;
    boolean b = true && false;
    b = true || false;
    b = 1 < 2;
    C c;
    C d;
    b = c == d;
  }
}

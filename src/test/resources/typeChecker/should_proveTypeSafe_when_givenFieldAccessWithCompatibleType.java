package typeChecker;

public class C {
  int a;
  boolean b;
  C c;

  void m() {
    this.a = 1;
    this.b = true;
    C d = this.c;
  }
}

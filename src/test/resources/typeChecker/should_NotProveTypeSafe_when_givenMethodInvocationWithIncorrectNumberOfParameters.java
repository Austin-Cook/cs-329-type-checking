package typeChecker;

public class C {
  int a(int a, boolean b) {
    return 1;
  }

  void m() {
    int i = a(1);
  }
}

public class feature_implicitthisclassforstaticmethods {
  public feature_implicitthisclassforstaticmethods() {}
  public static int m1() {
    return 42;
  }
  public int m2() {
    return m1();
  }
}


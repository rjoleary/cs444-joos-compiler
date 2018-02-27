
public class feature_nonstaticaccessofstaticmethod {
  public feature_nonstaticaccessofstaticmethod() {}
  public static int m1() {
    return 42;
  }
  public int m2() {
    feature_nonstaticaccessofstaticmethod a = new feature_nonstaticaccessofstaticmethod();
    return a.m1();
  }
}


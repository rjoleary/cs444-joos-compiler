
public class feature_nonstaticaccessofstaticfield {
  public feature_nonstaticaccessofstaticfield() {}
  public static int x;
  public int m() {
    feature_nonstaticaccessofstaticfield a = new feature_nonstaticaccessofstaticfield();
    return a.x;
  }
}


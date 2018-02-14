
public class feature_nonthisfieldaccess {
  public feature_nonthisfieldaccess() {}
  public int x;
  public void m() {
    new feature_nonthisfieldaccess().x = 42;
  }
}


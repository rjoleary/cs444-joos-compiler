
public class feature_trycatch {
  public feature_trycatch() {}
  public int m(java.io.FileInputStream x) {
    int y;
    try {
      y = x.read();
    } catch (java.io.IOException e) {
      y = 42;
    }
    return y;
  }
}


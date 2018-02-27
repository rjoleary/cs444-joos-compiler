
public class feature_finally {
  public feature_finally() {}
  public int m(java.io.FileInputStream x) throws java.io.IOException{
    int y;
    try {
      y = x.read();
    } catch (java.io.IOException e) {
      y = 42;
    } finally {
      x.close();
    }
    return y;
  }
}


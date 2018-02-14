
public class feature_nestedblocks {
  public feature_nestedblocks() {}
  public int m() {
    int x = 42;
    int y = 87;
    { int z = x+y;
      return z;
    }
  }
}


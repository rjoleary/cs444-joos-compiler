
public class feature_break {
  public feature_break() {}
  public int m(int x) {
    while (x>0) {
       x=x-1;
       if (x=42) break;
    }
    return x;
  }
}



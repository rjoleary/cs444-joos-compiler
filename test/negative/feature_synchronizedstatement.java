
public class feature_synchronizedstatement {
  public feature_synchronizedstatement() {}
  public int x;
  public void m() {
    synchronized(x) {
      x = x-1;
    }
}



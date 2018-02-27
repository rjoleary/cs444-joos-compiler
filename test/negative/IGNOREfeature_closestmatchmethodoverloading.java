
public class feature_closestmatchmethodoverloading {
  public feature_closestmatchmethodoverloading() {}
  public int m1(Object x, Object y) {
    return 42;
  }
  public int m1(Object x, feature_closestmatchmethodoverloading y) {
    return 87;
  }
  public int m2() {
    return this.m1(new feature_closestmatchmethodoverloading(), new feature_closestmatchmethodoverloading());
  }
}


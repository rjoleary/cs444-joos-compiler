
public class feature_superfieldaccess {
  public feature_superfieldaccess() {}
  public int f;
}

public class B extends feature_superfieldaccess {
  public B() {}
  public int m() { return super.f; }
}



public class feature_closestmatchconstructoroverloading {
  public feature_closestmatchconstructoroverloading() {}
  public feature_closestmatchconstructoroverloading(Object x, Object y) {}
  public feature_closestmatchconstructoroverloading(Object x, feature_closestmatchconstructoroverloading y) {}
  public void m() {
    new feature_closestmatchconstructoroverloading(new feature_closestmatchconstructoroverloading(), new feature_closestmatchconstructoroverloading());
  }
}


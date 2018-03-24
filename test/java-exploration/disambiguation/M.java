import A.C;
import A.F;

class M {

    String F = "Field shadows import";
    void nonStaticMethod () {
        System.out.println("== Non-static Method ==");

        System.out.println(F);

        System.out.println(C.s);
        String C = "Local shadows single-type import";
        System.out.println(C);

        System.out.println(A.C.s);
        String A = "Local shadows package name";
        System.out.println(A);

        System.out.println("============================");
    }
}

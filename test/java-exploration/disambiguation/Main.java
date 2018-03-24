import A.C;
import A.F;

public class Main {
    static String F = "Field shadows import";

    public static void main(String args[]) {
        System.out.println("== Static Method ==");

        System.out.println(F);

        System.out.println(C.s);
        String C = "Local shadows single-type import";
        System.out.println(C);

        System.out.println(A.C.s);
        String A = "Local shadows package name";
        System.out.println(A);

        System.out.println(P.C.D.p);

        System.out.println("============================\n");

        M m = new M();
        m.nonStaticMethod();
    }
}

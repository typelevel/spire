package spire.benchmark;

import java.math.*;

public class JavaFib {
    public static int intfib(int n) {
        int prev1 = 0, prev2 = 1;
        for(int i = 0; i < n; i++) {
            int savePrev1 = prev1;
            prev1 = prev2;
            prev2 = savePrev1 + prev2;
        }
        return prev1;
    }

    public static long longfib(int n) {
        long prev1 = 0L, prev2 = 1L;
        for(int i = 0; i < n; i++) {
            long savePrev1 = prev1;
            prev1 = prev2;
            prev2 = savePrev1 + prev2;
        }
        return prev1;
    }

    public static BigInteger bigfib(int n) {
        BigInteger prev1 = new BigInteger("0"), prev2 = new BigInteger("1");
        for(int i = 0; i < n; i++) {
            BigInteger savePrev1 = prev1;
            prev1 = prev2;
            prev2 = savePrev1.add(prev2);
        }
        return prev1;
    }
}

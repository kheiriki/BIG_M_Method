package bigM;

import java.util.Arrays;
import java.util.Scanner;

import static java.lang.System.exit;

class LinearProgramming {
    static final double EPSILON = 1.0E-10;
    static final double M = 1.0E+10;
    static double[][] a;   // tableaux
    static int m, n, l, g, e;  // number of constraints, variables, equations with <=, >= and =
    static int[] basis;


    public LinearProgramming(double[][] A, double[] b, double[] c, int[] sign, int func) {
        m = A.length;
        n = A[0].length;
        l = 0;
        g = 0;
        e = 0;

        for (int i = 0; i < m; i++) {
            if (sign[i] == -1) {
                l++;
            } else if (sign[i] == 1) {
                g++;
            } else {
                e++;
            }
        }

        int c_s = 0; //count of slack variables
        int c_e = 0; //count of excess variables
        int c_a = 0; //count of artificial variables

        a = new double[m + 1][n + l + 2 * g + e + 1];
        for (int i = 0; i < m; i++)
            for (int j = 0; j < n; j++)
                a[i][j] = A[i][j];
        for (int i = 0; i < m; i++) {
            if (sign[i] == -1) {
                a[i][n + c_s] = 1.0;
                c_s++;
            } else if (sign[i] == 1) {
                a[i][n + l + c_e] = -1.0;
                c_e++;
                a[i][n + l + g + c_a] = 1;
                c_a++;
            } else {
                a[i][n + l + g + c_a] = 1;
                c_a++;
            }
        }


        for (int i = 0; i < m; i++)
            a[i][n + l + 2 * g + e] = b[i];

        if (func == 1) {
            for (int j = 0; j < n; j++)
                a[m][j] = -1 * c[j];
            for (int j = n + l + g; j < n + l + 2 * g + e; j++) {
                a[m][j] = -1 * M;
                for (int i = 0; i < m; i++)
                    if (a[i][j] == 1)
                        pivot(i, j);
            }
        } else {
            for (int j = 0; j < n; j++)
                a[m][j] = -1 * c[j];
            for (int j = n + l + g; j < n + l + 2 * g + e; j++) {
                a[m][j] = M;
                for (int i = 0; i < m; i++)
                    if (a[i][j] == 1)
                        pivot(i, j);
            }
        }

        basis = new int[m];
        for (int i = 0; i < l; i++)
            basis[i] = n + i;
        for (int i = l; i < m; i++)
            basis[i] = n + g + i;
    }


    private static void pivot(int p, int q) {

        // everything but row p and column q
        for (int i = 0; i <= m; i++)
            for (int j = 0; j <= a[0].length - 1; j++)
                if (i != p && j != q) a[i][j] -= a[p][j] * a[i][q] / a[p][q];

        // zero out column q
        for (int i = 0; i <= m; i++)
            if (i != p) a[i][q] = 0.0;

        // scale row p
        for (int j = 0; j <= a[0].length - 1; j++)
            if (j != q) a[p][j] /= a[p][q];
        a[p][q] = 1.0;
    }

    private static int minDantzig() {
        int q = -1;
        double max = a[m][0];
        if (max > 0)
            q = 0;
        for (int j = 1; j < a[0].length - 1; j++) {

            if (a[m][j] > max) {
                max = a[m][j];
//                System.out.println("min =" +min);
                q = j;
//                System.out.println(" p =" + p);
            }
        }
        if (max > 0) {
            return q;
        } else
            return -1;
    }

    private static int maxDantzig() {
        int q = -1;
        double min = a[m][0];
        if (min < 0) ;
        q = 0;
        for (int j = 1; j < a[0].length - 1; j++) {

            if (a[m][j] < min) {
                min = a[m][j];
                q = j;
            }
        }
        if (min < 0) {
            return q;
        } else
            return -1;
    }

    private static int minRatioRule(int q) {
        int p = -1;
        double min = M;
        for (int i = 0; i < m; i++) {

            if (a[i][q] > EPSILON && a[i][a[0].length - 1] / a[i][q] < min) {
                min = a[i][a[0].length - 1] / a[i][q];
                p = i;
            }
        }
        return p;
    }

    private static boolean hasZero(int j) {
        if (a[m][j] == 0)
            return true;
        return false;
    }

    private static boolean hasZeroRHS(int i) {
        if (a[i][a[0].length - 1] == 0)
            return true;
        return false;
    }


    public static void main(String args[]) {
        System.out.println("Please enter a valid function type \n 1 for Min \t 2 for Max");
        Scanner input = new Scanner(System.in);
        int type = input.nextInt();
        while (type != 1 && type != 2) {
            System.out.println("Please enter a valid function type");
            type = input.nextInt();
        }
        //constraints count
        System.out.println("Enter the constraints count");
        int con = input.nextInt();
        //variables count
        System.out.println("Enter the variables count");

        int var = input.nextInt();
        double[][] A = new double[con][var];

        //creating A
        for (int i = 0; i < con; i++)
            for (int j = 0; j < var; j++) {
                System.out.println("Enter the coefficient of variable " + (j + 1) + " of constraint " + (i + 1));
                A[i][j] = input.nextDouble();
            }

        int[] sign = new int[con];

        //creating sign
        for (int i = 0; i < con; i++) {
            System.out.println("Enter the sign of constraint " + (i + 1) + "\n" + "-1 for <= \t 0 for = \t 1 for >=");
            sign[i] = input.nextInt();
        }

        double[] b = new double[con];

        //creating b
        for (int i = 0; i < con; i++) {
            System.out.println("Enter the bound of constraint " + (i + 1));
            b[i] = input.nextDouble();
        }

        double[] c = new double[var];

        //creating c
        for (int j = 0; j < var; j++) {
            System.out.println("Enter the coefficient of variable " + (j + 1) + " in cost function");
            c[j] = input.nextDouble();
        }


        bigM.LinearProgramming lp;

        lp = new bigM.LinearProgramming(A, b, c, sign, type);
        System.out.println(Arrays.deepToString(lp.a).replace("], ", "]\n"));
        System.out.println("--------------------------------------------------------------------------");
        int col = 0, row, checkFeasibility = 0, checkUnboundedness = 0, checkMultiple = 0, checkValue = 0;
        while (col != -1) {

            for (int i = 0; i < A.length; i++)
                //System.out.println(lp.basis[i]);

            if (type == 1)
                col = minDantzig();
            else
                col = maxDantzig();

            row = minRatioRule(col);
            if (row != -1) {
                basis[row] = col;
                pivot(row, col);
            } else {
                System.out.println("Unbounded");
                checkUnboundedness++;
                break;
            }
            for (int i = 0; i < a.length - 1; i++)
                if (a[i][a[0].length - 1] == 0 && a[i][col] > 0) {
                    System.out.println("Degenerate");
                    break;
                }
            System.out.println(Arrays.deepToString(lp.a).replace("], ", "]\n"));
            System.out.println("--------------------------------------------------------------------------");
            for (int i = 0; i < A.length; i++)

            if (type == 1)
                col = minDantzig();
            else
                col = maxDantzig();
        }
        for (int i = 0; i < A.length; i++)
            if (basis[i] >= n + l + g && basis[i] < a[0].length - 1 && a[i][a[0].length - 1] > 0)
                checkFeasibility++;
        if (checkFeasibility > 0) {
            System.out.println("Infeasible");
            exit(0);
        }


        if (checkUnboundedness == 0){
            System.out.println("Optimal Solution=" + a[a.length - 1][a[0].length - 1]);


        for (int j = 0; j < a[0].length - 1; j++)
            if (a[a.length - 1][j] == 0 && minRatioRule(j) != -1) {
                System.out.println("Multiple optimal solutions");
                exit(0);
            }

    }
}}
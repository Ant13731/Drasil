package Projectile;

/** \file InputParameters.java
    \author Samuel J. Crawford, Brooks MacLachlan, and W. Spencer Smith
    \brief Provides the structure for holding input values
*/
import java.util.Arrays;
import java.util.Scanner;
import java.io.PrintWriter;
import java.io.FileWriter;
import java.io.File;
import java.util.ArrayList;

/** \brief Structure for holding the input values
*/
public class InputParameters {
    public double v_launch;
    public double theta;
    public double p_target;
    
    /** \brief Initializes input object by reading inputs and checking physical constraints and software constraints on the input
        \param filename name of the input file
    */
    public InputParameters(String filename) throws Exception {
        this.get_input(filename);
        this.input_constraints();
    }
    
    /** \brief Reads input from a file with the given file name
        \param filename name of the input file
    */
    private void get_input(String filename) throws Exception {
        Scanner infile;
        infile = new Scanner(new File(filename));
        infile.nextLine();
        this.v_launch = Double.parseDouble(infile.nextLine());
        infile.nextLine();
        this.theta = Double.parseDouble(infile.nextLine());
        infile.nextLine();
        this.p_target = Double.parseDouble(infile.nextLine());
        infile.close();
    }
    
    /** \brief Verifies that input values satisfy the physical constraints and software constraints
    */
    private void input_constraints() throws Exception {
        if (!(this.v_launch > 0)) {
            System.out.print("Warning: ");
            System.out.print("v_launch has value ");
            System.out.print(this.v_launch);
            System.out.print(" but suggested to be ");
            System.out.print("above ");
            System.out.print(0);
            System.out.println(".");
        }
        if (!(0 < this.theta && this.theta < Math.PI / 2)) {
            System.out.print("Warning: ");
            System.out.print("theta has value ");
            System.out.print(this.theta);
            System.out.print(" but suggested to be ");
            System.out.print("between ");
            System.out.print(0);
            System.out.print(" and ");
            System.out.print(Math.PI / 2);
            System.out.print(" ((pi)/(2))");
            System.out.println(".");
        }
        if (!(this.p_target > 0)) {
            System.out.print("Warning: ");
            System.out.print("p_target has value ");
            System.out.print(this.p_target);
            System.out.print(" but suggested to be ");
            System.out.print("above ");
            System.out.print(0);
            System.out.println(".");
        }
    }
}

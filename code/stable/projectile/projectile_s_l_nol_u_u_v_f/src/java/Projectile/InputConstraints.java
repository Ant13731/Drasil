package Projectile;

/** \file InputConstraints.java
    \author Samuel J. Crawford, Brooks MacLachlan, and W. Spencer Smith
    \brief Provides the function for checking the physical constraints on the input
*/
public class InputConstraints {
    
    /** \brief Verifies that input values satisfy the physical constraints
        \param v_launch launch speed: the initial speed of the projectile when launched (m/s)
        \param theta launch angle: the angle between the launcher and a straight line from the launcher to the target (rad)
        \param p_target target position: the distance from the launcher to the target (m)
    */
    public static void input_constraints(float v_launch, float theta, float p_target) {
        if (!(v_launch > 0.0f)) {
            System.out.print("Warning: ");
            System.out.print("v_launch has value ");
            System.out.print(v_launch);
            System.out.print(", but is suggested to be ");
            System.out.print("above ");
            System.out.print(0.0f);
            System.out.println(".");
        }
        if (!(0.0f < theta && theta < Math.PI / 2.0f)) {
            System.out.print("Warning: ");
            System.out.print("theta has value ");
            System.out.print(theta);
            System.out.print(", but is suggested to be ");
            System.out.print("between ");
            System.out.print(0.0f);
            System.out.print(" and ");
            System.out.print(Math.PI / 2.0f);
            System.out.print(" ((pi)/(2))");
            System.out.println(".");
        }
        if (!(p_target > 0.0f)) {
            System.out.print("Warning: ");
            System.out.print("p_target has value ");
            System.out.print(p_target);
            System.out.print(", but is suggested to be ");
            System.out.print("above ");
            System.out.print(0.0f);
            System.out.println(".");
        }
    }
}

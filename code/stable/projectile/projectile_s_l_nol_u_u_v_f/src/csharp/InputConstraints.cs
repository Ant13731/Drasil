/** \file InputConstraints.cs
    \author Samuel J. Crawford, Brooks MacLachlan, and W. Spencer Smith
    \brief Provides the function for checking the physical constraints on the input
*/
using System;

public class InputConstraints {
    
    /** \brief Verifies that input values satisfy the physical constraints
        \param v_launch launch speed: the initial speed of the projectile when launched (m/s)
        \param theta launch angle: the angle between the launcher and a straight line from the launcher to the target (rad)
        \param p_target target position: the distance from the launcher to the target (m)
    */
    public static void input_constraints(float v_launch, float theta, float p_target) {
        if (!(v_launch > 0.0f)) {
            Console.Write("Warning: ");
            Console.Write("v_launch has value ");
            Console.Write(v_launch);
            Console.Write(", but is suggested to be ");
            Console.Write("above ");
            Console.Write(0.0f);
            Console.WriteLine(".");
        }
        if (!(0.0f < theta && theta < Math.PI / 2.0f)) {
            Console.Write("Warning: ");
            Console.Write("theta has value ");
            Console.Write(theta);
            Console.Write(", but is suggested to be ");
            Console.Write("between ");
            Console.Write(0.0f);
            Console.Write(" and ");
            Console.Write(Math.PI / 2.0f);
            Console.Write(" ((pi)/(2))");
            Console.WriteLine(".");
        }
        if (!(p_target > 0.0f)) {
            Console.Write("Warning: ");
            Console.Write("p_target has value ");
            Console.Write(p_target);
            Console.Write(", but is suggested to be ");
            Console.Write("above ");
            Console.Write(0.0f);
            Console.WriteLine(".");
        }
    }
}

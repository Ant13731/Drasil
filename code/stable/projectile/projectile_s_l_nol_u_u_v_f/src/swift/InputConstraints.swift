/** InputConstraints.swift
    Provides the function for checking the physical constraints on the input
    - Authors: Samuel J. Crawford, Brooks MacLachlan, and W. Spencer Smith
*/
/** Verifies that input values satisfy the physical constraints
    - Parameter v_launch: launch speed: the initial speed of the projectile when launched (m/s)
    - Parameter theta: launch angle: the angle between the launcher and a straight line from the launcher to the target (rad)
    - Parameter p_target: target position: the distance from the launcher to the target (m)
*/
func input_constraints(_ v_launch: Float, _ theta: Float, _ p_target: Float) -> Void {
    if !(v_launch > 0.0) {
        print("Warning: ", terminator: "")
        print("v_launch has value ", terminator: "")
        print(v_launch, terminator: "")
        print(", but is suggested to be ", terminator: "")
        print("above ", terminator: "")
        print(0.0, terminator: "")
        print(".")
    }
    if !(0.0 < theta && Double(theta) < Double.pi / Double(2.0)) {
        print("Warning: ", terminator: "")
        print("theta has value ", terminator: "")
        print(theta, terminator: "")
        print(", but is suggested to be ", terminator: "")
        print("between ", terminator: "")
        print(0.0, terminator: "")
        print(" and ", terminator: "")
        print(Double.pi / Double(2.0), terminator: "")
        print(" ((pi)/(2))", terminator: "")
        print(".")
    }
    if !(p_target > 0.0) {
        print("Warning: ", terminator: "")
        print("p_target has value ", terminator: "")
        print(p_target, terminator: "")
        print(", but is suggested to be ", terminator: "")
        print("above ", terminator: "")
        print(0.0, terminator: "")
        print(".")
    }
}

//MOC-FLAG --package pkg lib/pkg --package pkg-with-dependency lib/pkg-with-dependency --override lib/pkg-with-dependency dependency pkg
// Test the override from within the specified directory
import Pkg "mo:pkg-with-dependency";
Pkg.useDependency();

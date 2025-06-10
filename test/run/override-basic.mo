//MOC-FLAG --package pkg lib/pkg --package pkg-with-dependency lib/pkg-with-dependency --package dependency lib/pkg-with-dependency --override lib/pkg-with-dependency dependency pkg
import Pkg "mo:pkg-with-dependency";
Pkg.useDependency();

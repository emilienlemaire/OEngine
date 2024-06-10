(lang dune 3.14)

(name OEngine)
(generate_opam_files true)
(version 0.0.1)

(source
 (github emilienlemaire/oengine))

(authors "Emilien Lemaire <emilien.lem@icloud.com>")

(maintainers "Emilien Lemaire <emilien.lem@icloud.com>")

(license MIT)

(package
  (name OEngine_core)
  (synopsis "OEngine core utilities"))

(package
  (name OEngine_stubs)
  (synopsis "C stubs for OEngine"))

(package
  (name math)
  (synopsis "Math library for OpenGL/Vulkan"))

(package
  (name glfw)
  (synopsis "Glfw wrapper in OCaml")
  (depends
    (OEngine_core (= :version))))

(package
  (name glew)
  (synopsis "Glew wrapper in OCaml")
  (depends
    (OEngine_core (= :version))
    (OEngine_stubs (= :version))
    (glfw (= :version))))

(package
  (name gl)
  (synopsis "GL wrapper in OCaml")
  (depends
    (OEngine_core (= :version))
    (OEngine_stubs (= :version))
    ctypes))

(package
  (name OEngine_lib)
  (synopsis "Library for OEngine")
  (depends
    (OEngine_core (= :version))
    (glfw (= :version))
    (glew (= :version))
    imguiml))

(package
  (name OEngine)
  (synopsis "OEngine game engine")
  (description "OEngine game engine")
  (depends
    (OEngine_lib (= :version))))

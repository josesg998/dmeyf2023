# Repositorios remotos

En general los sistemas de versionado se utilizan para trabajar
colaborativamente. Para eso uno tiene que publicar su código en algun lado
para que otros lo vean y puedan usarlo y agregar sus cambios.

Rara vez uno tiene un repositorio solo. En general lo usa en algun servidor
como GitHub o GitLab. Para eso git tiene un manejo de repositorios remotos con
`git remote`.

Dentro de nuestro repo probemos:

```shell
$ git remote
$
```

No pasó nada? Agreguemos una `-v` (de verborragia):
```shell
$ git remote -v
$
```

Nada? Bueno, tiene sentido. Nuestro repositorio es local y nunca lo bajamos de
otro lado ni lo publicamos. Probemos tomando un repositorio de algun otro lado

# Clonando repositorios

Busquemos uno en GitHub: el de la materia, por supuesto! https://github.com/dmecoyfin/dmeyf2023

Navegamos a algún lugar de nuestros directorios (uno que no sea ya un
repositorio git) y hacemos `git clone`:
```shell
$ git clone https://github.com/dmecoyfin/dmeyf2023.git
$ cd dmeyf2023
```

Nota: para la materia van a tener que tener su propio repositorio derivado
(fork) del de la materia. Estos pasos son ilustrativos.

Ahora probemos de vuelta que pasa con los remotos:

```shell
$ git remote
origin
$
```

Bueno es algo. Sabemos que existe un remoto que se llama `origin`. Es el
nombre por defecto del remoto de donde se clonó el repo. También podemos ver
más detalle agregando `-v`:

```shell
$ git remote -v
origin  https://github.com/dmecoyfin/dmeyf2023.git (fetch)
origin  https://github.com/dmecoyfin/dmeyf2023.git (push)
```

# Ramas

Es en general buena práctica no hacer commits directos a la rama principal
(`main` o `master`) de un repositorio remoto. Uno hace su código en una rama
propia "descartable" y luego mezcla (`merge`) los cambios a la rama principal.

Pero qué son las ramas? Hablamos de que git maneja un árbol de versiones. Uno
puede tener varias versiones basadas en versiones anteriores y que éstas
diverjan desde un punto común. Se dice que el árbol se dividió en ramas.

Para crear ramas uno puede hacerlo con el commando `git branch`:

```shell
$ git branch mi-version-alternativa
$
```

Con esto, git creó la rama pero no cambió a ella. Para eso hay que usar el
comando `git checkout`:

```shell
$ git checkout mi-version-alternativa
Cambiado a rama 'mi-version-alternativa'
$ git status
En la rama mi-version-alternativa
nada para hacer commit, el árbol de trabajo está limpio
```

> ;) Y qué pasaría si usamos nuestro `git tree` ahora?

Todos los commits que hagamos ahora se harán sobre la rama actual sin afectar
al resto.

# Reconectar ramas

Ahora qué pasa cuando uno quiere incorporar los cambios de una rama a la
principal? Fácil, se usa el comando `git merge`. Pero tiene unas cositas
a tener en cuenta. Uno debe estar en la rama a la que quiere traer los cambios
(moverse con `git checkout` y verificar con `git status`). Luego usar
`git merge <nombre de rama a traer>`. Git hará lo posible por mezclar los
cambios. Si surge un conflicto en el `git status` aparecerá y nos dará las
instrucciones de como avanzar una vez resuelto el conflicto o como deshacer el
merge.

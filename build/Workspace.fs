module EasyBuild.Workspace

open EasyBuild.FileSystemProvider

[<Literal>]
let root = __SOURCE_DIRECTORY__ + "/../"

type Workspace = RelativeFileSystem<root>

type VirtualWorkspace =
    VirtualFileSystem<
        root,
        """
.fsdocs/
tests/
    fableBuild/
web/
    Docs/
        dist/
output-fsdocs/
deploy/
    sources/
"""
     >

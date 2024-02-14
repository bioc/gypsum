# Creating a test project and populating it for use in examples and tests.

createProject(
    "test-R", 
    owners="LTLA", 
    uploaders=list(list(id="ArtifactDB-bot"))
)

# Uploading the first version.
tmp <- tempfile()
dir.create(tmp)
write(file=file.path(tmp, "blah.txt"), LETTERS)
dir.create(file.path(tmp, "foo"))
write(file=file.path(tmp, "foo", "bar.txt"), 1:10)

blob <- startUpload(
    project="test-R", 
    asset="basic", 
    version="v1", 
    files=list.files(tmp, recursive=TRUE),
    directory=tmp
)
uploadFiles(blob, directory=tmp)
completeUpload(blob)

# Uploading a second version with implicit links.
blob <- startUpload(
    project="test-R", 
    asset="basic", 
    version="v2", 
    files=list.files(tmp, recursive=TRUE),
    directory=tmp
)
completeUpload(blob)

# Uploading a third version with implicit links to the ancestral v1.
blob <- startUpload(
    project="test-R", 
    asset="basic", 
    version="v3", 
    files=list.files(tmp, recursive=TRUE),
    directory=tmp
)
completeUpload(blob)

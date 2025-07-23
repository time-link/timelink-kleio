Files to test the new yaml structures

How to test:

1. Place the kleio files to test in this directory
   (e.g., `tests/kleio-home/sources/reference_sources/yaml/pt-groups.kleio`)
2. Place the yaml structures in tests/kleio-home/structures/api/yaml/
   (e.g., `tests/kleio-home/structures/api/yaml/pt-groups.yaml`)
   1. Structure iwth the same name as the kleio file will be used for translation.
3. Load the serverStart.pl file in your Prolog environment
4. run_test(server). Will copy the kleio files in this directory to sources/api/yaml and then use the structutures in structures/api/yaml to translate the kleio files.

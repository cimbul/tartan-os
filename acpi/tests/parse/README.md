# Parser snapshot tests

The integration tests in this directory serve two purposes:

  1. Verifying what constructs are allowed by the ACPICA parser, in cases where the
     grammar in the spec is unclear or obviously wrong—which happens far too often.
  2. Regression testing the Tartan ACPI parser without having to manually craft AML and
     write complex assertions. (There is plenty of that in the unit tests already.)

The files under the `aml/` folder were generated from the corresponding files in the
`asl/` directory using the ACPICA (Intel) ASL compiler. These tests run the Tartan ACPI
parser on each of the ASL files and compare its output to a previous snapshot recorded by
the [`insta`](https://docs.rs/insta/0.16.1/insta/) tool.

You do **not** need any external tools to run the integration tests. However, if you
want to modify the AML files or manage the snapshots, you will need:

  * bash 3.0+
  * ACPICA version 20200717 or higher, specifically the `iasl` tool.
  * Optionally, `cargo-insta` for more easily reviewing and updating snapshots.


## Editing ASL

You do **not** need to have ACPICA installed in order to run the integration tests,
because they only reference the `aml/` folder. However, you will need ACPICA if you want
to update the AML files.

After creating or editing an ASL file, regenerate the AML with:

```bash
compile-all.sh
```


## Updating snapshots

After making a change to the AML, rerun the tests with:

```bash
cargo test
```

The tests should fail and warn you about the changed output. Review the changes and update
the snapshots if appropriate:

```bash
cargo insta review

# Or manually:
mv snapshots/parse__<FILE>.snap.new \
   snapshots/parse__<FILE>.snap
```

For more information on managing snapshots, see the
[`insta` documentation](https://docs.rs/insta/0.16.1/insta/).

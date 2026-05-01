# fpdwg inspector developer notes

The inspector is split into small layers:

1. Reader loads LibreDWG and owns the `Dwg_Data` lifetime.
2. Factory and mappers copy scalar fields into domain objects.
3. Registry indexes every object by handle before references are resolved.
4. Resolver connects handles to objects and assigns object status.
5. Validator records broken references, orphan objects, and owner cycles.
6. Reporters render text or JSON from the domain model.

Mappers must not print to stdout, resolve references during allocation, or keep
pointers into LibreDWG memory after the reader frees `Dwg_Data`.

## How to add a new DWG object type

1. Add or extend a domain model class under `inspector/model/`.
   Entities should inherit `TDWGEntity`; non-entity records should inherit
   `TDWGObject` or `TDWGTableRecord`.
2. Implement a mapper in `inspector/mappers/` that implements
   `IDWGObjectMapper`.
3. In the mapper, copy only scalar fields and handle references. Use
   `HandleRefFromBitCode` for `BITCODE_H` values and `SafeDecodeText` for all
   strings from LibreDWG.
4. Register the mapper in `TDWGObjectFactory.CreateDefault`, for example:

   ```pascal
   Result.RegisterMapper(DWG_TYPE_CIRCLE, TDWGCircleMapper.Create);
   ```

5. Add reporter output in `fpdwg_reporter.pp` for text and JSON if the object
   should appear as a first-class report section.
6. Extend CLI filtering in `fpdwg_cli.pp` or `fpdwg_filter.pp` only if users
   need to materialize that type selectively.
7. Add unit tests for mapper field copying, deferred handle resolution, and
   reporter output. Use `mappers/fpdwg_map_line.pp` and
   `tests/fpdwg_test_factory.pp` as the current reference pattern.

## Test commands

From `cad_source/components/fpdwg/inspector/tests/`:

```sh
fpc -Fu.. -Fu../.. -Fu../model -Fu../mappers fpdwg_tests.lpr
./fpdwg_tests --all --format=plain
```

From `cad_source/components/fpdwg/fpdwginspect/`:

```sh
fpc -Fu.. -Fu../inspector -Fu../inspector/model -Fu../inspector/mappers fpdwginspect.lpr
./fpdwginspect --help
```

The smoke tests inject a fake LibreDWG API at the reader boundary so CI can
exercise the full inspector pipeline for R2000 and R2007 DWG version headers
without requiring `libredwg.so`. Manual fixture checks with real DWG files are
still needed when LibreDWG is installed.

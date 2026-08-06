# v0.5.1.1

- Support `hasql-2`: widened the `hasql` dependency bound to `>=1.10 && <1.11 || >=2.0 && <2.1`

# v0.5

- (Breaking) Removed `Hasql.DynamicStatements.Session` module
- (Breaking) Removed `Hasql.DynamicStatements.Statement` module
- Added `toStatement`, `toSession`, `toPipeline` functions to the `Hasql.DynamicStatements.Snippet` module instead

# v0.4

- Migrated to Hasql 1.10
- (Breaking) Redefined `Snippet.sql` to use `Text` instead of `ByteString`
- (Breaking) Dropped the preparability flags assuming that all dynamic statements should not be
  prepared

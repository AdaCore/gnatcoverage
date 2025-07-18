#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Demangle/Demangle.h"
#include "llvm/ProfileData/Coverage/CoverageMapping.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/Error.h"
#include "llvm/Support/JSON.h"
#include "llvm/Support/VirtualFileSystem.h"
#include <iterator>
#include <type_traits>
#include <vector>

using namespace llvm;

enum MCDCState
{
  DontCare = -1,
  False = 0,
  True = 1,
};

std::string
dump_kind (const coverage::CounterMappingRegion &region)
{
  switch (region.Kind)
    {
    case coverage::CounterMappingRegion::CodeRegion:
      return "CodeRegion";
    case coverage::CounterMappingRegion::ExpansionRegion:
      return "ExpansionRegion";
    case coverage::CounterMappingRegion::SkippedRegion:
      return "SkippedRegion";
    case coverage::CounterMappingRegion::GapRegion:
      return "GapRegion";
    case coverage::CounterMappingRegion::BranchRegion:
      return "BranchRegion";
    case coverage::CounterMappingRegion::MCDCDecisionRegion:
      return "MCDCDecisionRegion";
    case coverage::CounterMappingRegion::MCDCBranchRegion:
      return "MCDCBranchRegion";
    }
}

// Create a JSON array starting of a vector of objects.
// F is the conversion function, should be of type `json::Object F(T &)`
template <class T, class Fct>
json::Array
dump_array (const std::vector<T> &regions, Fct &&fct)
{
  json::Array result;

  std::transform (regions.begin (), regions.end (),
                  std::back_inserter (result),
                  std::function (std::forward<Fct> (fct)));

  return result;
}

json::Array
dump_location_span (const coverage::CounterMappingRegion &mapping_region)
{
  auto start = mapping_region.startLoc ();
  auto end = mapping_region.endLoc ();
  return json::Array ({ start.first, start.second, end.first, end.second });
}

json::Array
dump_test_vectors (coverage::MCDCRecord &mcdc_record)
{
  json::Array test_vectors;

  for (auto tv_index = 0; tv_index < mcdc_record.getNumTestVectors ();
       ++tv_index)
    {
      json::Array tv_conditions;
      for (auto condition_index = 0;
           condition_index < mcdc_record.getNumConditions ();
           ++condition_index)
        {
          MCDCState state;
          switch (mcdc_record.getTVCondition (tv_index, condition_index))
            {
            case coverage::MCDCRecord::MCDC_DontCare:
              state = MCDCState::DontCare;
              break;
            case coverage::MCDCRecord::MCDC_False:
              state = MCDCState::False;
              break;
            case coverage::MCDCRecord::MCDC_True:
              state = MCDCState::True;
              break;
            }
          tv_conditions.push_back (static_cast<int16_t> (state));
        }

      auto decision_result = mcdc_record.getTVResult (tv_index);
      test_vectors.push_back (json::Object (
        { { "vector", std::move (tv_conditions) },
          { "decision_outcome", static_cast<int16_t> (decision_result) } }));
    }
  return test_vectors;
}

json::Object
dump_mcdc_record (const coverage::MCDCRecord &mcdc_record)
{
  return json::Object (
    { { "span", dump_location_span (mcdc_record.getDecisionRegion ()) },
      { "num_conditions", mcdc_record.getNumConditions () },
      { "test_vectors", dump_test_vectors (const_cast<coverage::MCDCRecord &> (
                          mcdc_record)) } });
}

json::Object
dump_branch_region (const coverage::CountedRegion &region)
{
  return json::Object ({ { "span", dump_location_span (region) },
                         { "true_count", region.ExecutionCount },
                         { "false_count", region.FalseExecutionCount },
                         { "kind", dump_kind (region) } });
}

json::Object
dump_code_region (const coverage::CountedRegion &region)
{
  return json::Object ({ { "span", dump_location_span (region) },
                         { "count", region.ExecutionCount },
                         { "kind", dump_kind (region) } });
}

json::Object
dump_function (const coverage::FunctionRecord &fct)
{
  return json::Object (
    { { "name", fct.Name },
      { "demangled_name", rustDemangle (std::string_view (fct.Name)) },
      { "mcdc_records", dump_array (fct.MCDCRecords, dump_mcdc_record) },
      { "branch_regions",
        dump_array (fct.CountedBranchRegions, dump_branch_region) },
      { "code_regions", dump_array (fct.CountedRegions, dump_code_region) } });
}

json::Object
dump_coverage_for_source_file (const coverage::CoverageMapping &coverage,
                               StringRef sourcefile)
{
  json::Array functions;

  auto fct_iterator = coverage.getCoveredFunctions (sourcefile);
  std::transform (fct_iterator.begin (), fct_iterator.end (),
                  std::back_inserter (functions), dump_function);

  return json::Object ({ { "filename", sourcefile.str () },
                         { "functions", std::move (functions) } });
}

json::Object
dump_coverage (const coverage::CoverageMapping &coverage)
{
  json::Array file_coverages
    = dump_array (coverage.getUniqueSourceFiles (), [&] (StringRef filename) {
        return dump_coverage_for_source_file (coverage, filename);
      });

  return json::Object ({ { "data", std::move (file_coverages) } });
}

int
main (int argc, const char *argv[])
{
  cl::opt<std::string> profdata_filename (
    "instr-prof", cl::Required, cl::desc (".profdata file location"));
  cl::opt<std::string> object_filename (cl::Positional, cl::Required,
                                        cl::desc ("object file location"));
  cl::ParseCommandLineOptions (argc, argv,
                               "GNATcov LLVM profiling data exporting tool");

  auto FS = vfs::getRealFileSystem ();

  auto coverage_or_err = coverage::CoverageMapping::load (
    { object_filename }, profdata_filename, *FS);

  if (Error e = coverage_or_err.takeError ())
    {
      errs () << "Failed to load coverage: " << toString (std::move (e))
              << '\n';
      return EXIT_FAILURE;
    }

  auto coverage = std::move (coverage_or_err->get ());

  auto json_export = dump_coverage (*coverage);

  outs () << std::move (json_export);

  return EXIT_SUCCESS;
}

package purescript

import (
	"bufio"
	"flag"
	"fmt"
	"io"
	"log"
	"os"
	"path/filepath"
	"sort"
	"strings"

	"github.com/bazelbuild/bazel-gazelle/config"
	"github.com/bazelbuild/bazel-gazelle/label"
	"github.com/bazelbuild/bazel-gazelle/language"
	"github.com/bazelbuild/bazel-gazelle/repo"
	"github.com/bazelbuild/bazel-gazelle/resolve"
	"github.com/bazelbuild/bazel-gazelle/rule"
)

type pureScript struct {
	modules map[string]label.Label
}

type pureScriptModule struct {
	module  string
	imports []string
}

func findImports(scanner *bufio.Scanner) []string {
	var imports []string
	for scanner.Scan() {
		parsedImport, parsedImportOk := parseImport(scanner.Text())
		if parsedImportOk {
			imports = append(imports, parsedImport)
		}
	}
	sort.Strings(imports)

	return imports
}

func findModule(scanner *bufio.Scanner) (string, bool) {
	var module string
	var ok bool
	for scanner.Scan() {
		module, ok = parseModule(scanner.Text())
		if ok {
			break
		}
	}
	return module, ok
}

func parseImport(str string) (string, bool) {
	tokens := strings.Fields(str)
	if len(tokens) > 1 && tokens[0] == "import" {
		return tokens[1], true
	}
	return "", false
}

func parseModule(str string) (string, bool) {
	tokens := strings.Fields(str)
	if len(tokens) > 1 && tokens[0] == "module" {
		return tokens[1], true
	}
	return "", false
}

func parsePureScriptModule(reader io.Reader) (pureScriptModule, error) {
	scanner := bufio.NewScanner(reader)
	parsedModule, parsedModuleOk := findModule(scanner)
	if parsedModuleOk {
		return pureScriptModule{
			module:  parsedModule,
			imports: findImports(scanner),
		}, nil
	}
	return pureScriptModule{}, fmt.Errorf("Could not parse module")
}

// RegisterFlags registers command-line flags used by the extension. This
// method is called once with the root configuration when Gazelle
// starts. RegisterFlags may set an initial values in Config.Exts. When flags
// are set, they should modify these values.
func (p *pureScript) RegisterFlags(fs *flag.FlagSet, cmd string, c *config.Config) {}

// CheckFlags validates the configuration after command line flags are parsed.
// This is called once with the root configuration when Gazelle starts.
// CheckFlags may set default values in flags or make implied changes.
func (p *pureScript) CheckFlags(fs *flag.FlagSet, c *config.Config) error {
	return nil
}

// KnownDirectives returns a list of directive keys that this Configurer can
// interpret. Gazelle prints errors for directives that are not recoginized by
// any Configurer.
func (p *pureScript) KnownDirectives() []string {
	return []string{}
}

// Configure modifies the configuration using directives and other information
// extracted from a build file. Configure is called in each directory.
//
// c is the configuration for the current directory. It starts out as a copy
// of the configuration for the parent directory.
//
// rel is the slash-separated relative path from the repository root to
// the current directory. It is "" for the root directory itself.
//
// f is the build file for the current directory or nil if there is no
// existing build file.
func (p *pureScript) Configure(c *config.Config, rel string, f *rule.File) {}

// Name returns the name of the language. This should be a prefix of the
// kinds of rules generated by the language, e.g., "go" for the Go extension
// since it generates "go_library" rules.
func (p *pureScript) Name() string {
	return "purescript"
}

// Imports returns a list of ImportSpecs that can be used to import the rule
// r. This is used to populate RuleIndex.
//
// If nil is returned, the rule will not be indexed. If any non-nil slice is
// returned, including an empty slice, the rule will be indexed.
func (p *pureScript) Imports(c *config.Config, r *rule.Rule, f *rule.File) []resolve.ImportSpec {
	switch r.Kind() {
	case "purescript_library":
		return p.purescriptLibraryImports(c, r, f)
	default:
		return nil
	}
}

func (p *pureScript) purescriptLibraryImports(c *config.Config, r *rule.Rule, f *rule.File) []resolve.ImportSpec {
	return []resolve.ImportSpec{
		{
			Lang: p.Name(),
			Imp:  r.Name(),
		},
	}
}

// Embeds returns a list of labels of rules that the given rule embeds. If
// a rule is embedded by another importable rule of the same language, only
// the embedding rule will be indexed. The embedding rule will inherit
// the imports of the embedded rule.
func (p *pureScript) Embeds(r *rule.Rule, from label.Label) []label.Label {
	return []label.Label{}
}

// Resolve translates imported libraries for a given rule into Bazel
// dependencies. Information about imported libraries is returned for each
// rule generated by language.GenerateRules in
// language.GenerateResult.Imports. Resolve generates a "deps" attribute (or
// the appropriate language-specific equivalent) for each import according to
// language-specific rules and heuristics.
func (p *pureScript) Resolve(c *config.Config, ix *resolve.RuleIndex, rc *repo.RemoteCache, r *rule.Rule, imports interface{}, from label.Label) {
	switch r.Kind() {
	case "purescript_library":
		p.purescriptLibraryResolve(c, ix, rc, r, imports, from)
	default:
		return
	}
}

func (p *pureScript) purescriptLibraryResolve(c *config.Config, ix *resolve.RuleIndex, rc *repo.RemoteCache, r *rule.Rule, imports interface{}, from label.Label) {
	moduleNames := imports.([]string)
	deps := make([]string, 0, len(moduleNames))

	for _, moduleName := range moduleNames {
		moduleLabel, ok := p.modules[moduleName]
		if ok {
			dep := moduleLabel.Rel(from.Repo, from.Pkg)
			deps = append(deps, dep.String())
		}
	}

	if len(deps) != 0 {
		r.SetAttr("deps", deps)
	}
}

// Kinds returns a map of maps rule names (kinds) and information on how to
// match and merge attributes that may be found in rules of those kinds. All
// kinds of rules generated for this language may be found here.
func (p *pureScript) Kinds() map[string]rule.KindInfo {
	return map[string]rule.KindInfo{
		"purescript_library": p.purescriptLibraryKinds(),
	}
}

func (p *pureScript) purescriptLibraryKinds() rule.KindInfo {
	return rule.KindInfo{
		MatchAttrs: []string{
			"module",
		},
		MergeableAttrs: map[string]bool{
			"ffi": true,
			"src": true,
		},
		NonEmptyAttrs: map[string]bool{
			"ffi": true,
			"src": true,
		},
		ResolveAttrs: map[string]bool{
			"deps": true,
		},
	}
}

// Loads returns .bzl files and symbols they define. Every rule generated by
// GenerateRules, now or in the past, should be loadable from one of these
// files.
func (p *pureScript) Loads() []rule.LoadInfo {
	return []rule.LoadInfo{
		{
			Name: label.New("joneshf_rules_purescript", "purescript", "defs.bzl").String(),
			Symbols: []string{
				"purescript_library",
			},
		},
	}
}

// GenerateRules extracts build metadata from source files in a directory.
// GenerateRules is called in each directory where an update is requested
// in depth-first post-order.
//
// args contains the arguments for GenerateRules. This is passed as a
// struct to avoid breaking implementations in the future when new
// fields are added.
//
// A GenerateResult struct is returned. Optional fields may be added to this
// type in the future.
//
// Any non-fatal errors this function encounters should be logged using
// log.Print.
func (p *pureScript) GenerateRules(args language.GenerateArgs) language.GenerateResult {
	result := language.GenerateResult{}

	if bowerNonSource(args.Rel) || nodeModulesNonSource(args.Rel) || spagoNonSource(args.Rel) {
		return result
	}

	for _, regularFile := range args.RegularFiles {
		if isPureScriptFile(regularFile) {
			isDependency := bowerSource(args.Rel) || spagoSource(args.Rel)
			p.generatePureScriptRules(args, regularFile, isDependency, &result)
		}
	}

	if args.File == nil {
		return result
	}

	for _, r := range args.File.Rules {
		switch r.Kind() {
		case "purescript_library":
			p.removeUnusedPureScriptLibraryRule(r, &result)
		default:
		}

	}

	return result
}

// bazelizePureScriptModuleName performs any cleanup of the PureScript module name to something that bazel accepts as a target.
// Valid PureScript module names should also be valid bazel targets,
// so we're not doing a whole lot here.
//
// This mainly exists as a place to slot in behavior should we need it.
func bazelizePureScriptModuleName(moduleName string) string {
	return moduleName
}

// bowerNonSource checks if a path is likely from `bower`,
// but isn't a source path.
//
// We're currently guessing that the files we want are in the `bower_components/purescript-*/src` directory.
func bowerNonSource(filename string) bool {
	paths := strings.Split(filename, "/")

	return len(paths) >= 3 &&
		paths[0] == "bower_components" &&
		strings.HasPrefix(paths[1], "purescript-") &&
		paths[2] != "src"
}

// bowerSource checks if a path is likely from `bower`,
// and is a source path.
//
// We're currently guessing that the files we want are in the `bower_components/purescript-*/src` directory.
func bowerSource(filename string) bool {
	paths := strings.Split(filename, "/")

	return len(paths) >= 3 &&
		paths[0] == "bower_components" &&
		strings.HasPrefix(paths[1], "purescript-") &&
		paths[2] == "src"
}

func (p *pureScript) generatePureScriptRules(args language.GenerateArgs, pureScriptFilename string, isDependency bool, result *language.GenerateResult) {
	file, err := os.Open(filepath.Join(args.Rel, pureScriptFilename))
	if err != nil {
		log.Print(err)
		return
	}
	defer file.Close()

	psModule, err := parsePureScriptModule(file)
	if err != nil {
		log.Print(err)
		return
	}

	name := bazelizePureScriptModuleName(psModule.module)

	p.modules[psModule.module] = label.New(args.Config.RepoName, args.Rel, name)

	r := rule.NewRule("purescript_library", name)

	r.SetAttr("module", psModule.module)
	r.SetAttr("src", pureScriptFilename)
	r.SetAttr("visibility", []string{
		"//visibility:public",
	})

	if isDependency {
		r.SetAttr("ignore_warnings", true)
	}

	basename := strings.TrimSuffix(pureScriptFilename, ".purs")
	ffiFilename := fmt.Sprintf("%s.js", basename)
	_, err = os.Stat(filepath.Join(args.Rel, ffiFilename))
	if !os.IsNotExist(err) {
		r.SetAttr("ffi", ffiFilename)
	}

	imports := make([]string, 0, len(psModule.imports))
	for _, import_ := range psModule.imports {
		imports = append(imports, import_)
	}
	result.Imports = append(result.Imports, imports)

	result.Gen = append(result.Gen, r)
}

func isPureScriptFile(filename string) bool {
	return filepath.Ext(filename) == ".purs"
}

// nodeModulesNonSource checks if a path is likely from `npm`.
func nodeModulesNonSource(filename string) bool {
	paths := strings.Split(filename, "/")
	return len(paths) >= 1 &&
		paths[0] == "node_modules"
}

func (p *pureScript) removeUnusedPureScriptLibraryRule(r *rule.Rule, result *language.GenerateResult) {
	_, moduleNameExists := p.modules[r.AttrString("module")]
	if moduleNameExists {
		return
	}

	result.Empty = append(result.Empty, rule.NewRule("purescript_library", r.Name()))
}

// spagoNonSource checks if a path is likely from `spago`,
// but isn't a source path.
//
// We're currently guessing that the files we want are in the `.spago/*/*/src` directory.
func spagoNonSource(filename string) bool {
	paths := strings.Split(filename, "/")
	return len(paths) >= 4 &&
		paths[0] == ".spago" &&
		paths[3] != "src"
}

// spagoSource checks if a path is likely from `spago`,
// and is a source path.
//
// We're currently guessing that the files we want are in the `.spago/*/*/src` directory.
func spagoSource(filename string) bool {
	paths := strings.Split(filename, "/")
	return len(paths) >= 4 &&
		paths[0] == ".spago" &&
		paths[3] == "src"
}

// Fix repairs deprecated usage of language-specific rules in f. This is
// called before the file is indexed. Unless c.ShouldFix is true, fixes
// that delete or rename rules should not be performed.
func (p *pureScript) Fix(c *config.Config, f *rule.File) {}

func NewLanguage() language.Language {
	return &pureScript{
		modules: make(map[string]label.Label),
	}
}

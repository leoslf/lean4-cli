import Cli.Basic

namespace Cli

section Extensions
  /-- Prepends an author name to the description of the command. -/
  def author (author : String) : Extension := {
      extend := fun cmd => cmd.update (description := s!"{author}\n{cmd.description}")
    }

  /-- Appends a longer description to the end of the help. -/
  def longDescription (description : String) : Extension := {
      extend := fun cmd => cmd.update (furtherInformation? :=
        some <| Option.optStr cmd.furtherInformation? ++ lines #[
          Option.optStr cmd.furtherInformation?,
          (if cmd.hasFurtherInformation then "\n" else "") ++ renderSection "DESCRIPTION" description
        ]
      )
    }

  /-- Adds a `help` subcommand. -/
  def helpSubCommand : Extension := {
      priority := 0
      extend   := fun cmd =>
        let helpCmd := .mk
          (parent      := cmd)
          (name        := "help")
          (version?    := none)
          (description := "Prints this message.")
          (run         := fun _ => pure 0)
        -- adding it once without a command handler ensures that the help will include
        -- the help subcommand itself
        let cmd := cmd.update (subCmds := cmd.subCmds.push helpCmd)
        let helpCmd := helpCmd.update (run := fun _ => do
          cmd.toFullCmdWithoutExtensions.printHelp
          return 0)
        let subCmds := cmd.subCmds.set! (cmd.subCmds.size - 1) helpCmd
        cmd.update (subCmds := subCmds)
    }

  /-- Adds a `version` subcommand. -/
  def versionSubCommand! : Extension := {
    extend := fun cmd =>
      if cmd.version?.isNone then
        panic! "Cli.versionSubCommand!: Cannot add `version` subcommand to command without a version."
      else
        let helpCmd := .mk
          (parent      := cmd)
          (name        := "version")
          (version?    := none)
          (description := "Prints the version.")
          (run         := fun _ => do
            cmd.toFullCmdWithoutExtensions.printVersion!
            return 0)
        cmd.update (subCmds := cmd.subCmds.push helpCmd)
  }

  /--
  Sets default values for flags that were not set by the user according to
  `defaults := #[(long flag name, default value), ...]` and denotes the default value
  in the flag description of the help.
  Panics if one of the designated long flag names cannot be found in the command.
  -/
  def defaultValues! (defaults : Array (String × String)) : Extension :=
    let findDefaultFlags cmd := defaults.map <| fun (longName, defaultValue) =>
      ⟨cmd.flag! longName, defaultValue, .default⟩
    {
      extend := fun cmd =>
        let defaultFlags := findDefaultFlags cmd
        let newMetaFlags := cmd.flags.map fun flag =>
          if let some defaultFlag := defaultFlags.find? (·.flag.longName = flag.longName) then
            { flag with description := flag.description ++ s!" [Default: `{defaultFlag.value}`]" }
          else
            flag
        cmd.update (flags := newMetaFlags)
      postprocess := fun cmd parsed =>
        let defaultFlags := findDefaultFlags cmd
        return { parsed with flags := Array.leftUnionBy (·.flag.longName) parsed.flags defaultFlags }
    }

  /--
  Errors if one of `requiredFlags := #[long flag name, ...]` were not passed by the user.
  Denotes that the flag is required in the flag description of the help.
  Panics if one of the designated long flag names cannot be found in the command.
  -/
  def require! (requiredFlags : Array String) : Extension :=
    let findRequiredFlags cmd := requiredFlags.map (cmd.flag! ·)
    {
      extend := fun cmd =>
        let requiredFlags := findRequiredFlags cmd
        let newMetaFlags := cmd.flags.map fun flag =>
          if requiredFlags.find? (·.longName = flag.longName) |>.isSome then
            { flag with description := "[Required] " ++ flag.description }
          else
            flag
        cmd.update (flags := newMetaFlags)
      postprocess := fun cmd parsed => do
        if parsed.hasFlag "help" ∨ parsed.hasFlag "version" then
          return parsed
        let requiredFlags := findRequiredFlags cmd
        let missingFlags := Array.diffBy (·.longName) requiredFlags <| parsed.flags.map (·.flag.longName)
        if let some missingFlag ← pure <| missingFlags[0]? then
          throw s!"Missing required flag `--{missingFlag.longName}`."
        return parsed
    }

  def envVars : Extension :=
    {
      extend := λcmd =>
        let flags' := cmd.flags |>.map λflag =>
          match flag.envVar? with
          | .none => flag
          | .some envVar =>
            { flag with description := flag.description ++ s!" [env: {envVar}]" }
        cmd.update (flags := flags')
      postprocess := λcmd parsed => do
        let flags' : Array Parsed.Flag ← cmd.flags.filterMapM λflag => do
          let some envVar := flag.envVar?
            | return none
          let some value ← IO.getEnv envVar
            | return none
          return some ⟨flag, value, .envVar⟩
        return { parsed with flags := Array.leftUnionBy (·.flag.longName) parsed.flags flags' }
    }
end Extensions

end Cli

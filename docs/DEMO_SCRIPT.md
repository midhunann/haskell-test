# 🎬 Haskell Run Extension Demo Recording Script

## 📋 Pre-Recording Setup
1. Open VS Code in the `haskell-test` folder
2. Install the Haskell Run extension (if not already installed)
3. Make sure GHC/GHCi is installed and available in PATH
4. Set up screen recording software
5. Prepare VS Code with clean interface (close unnecessary panels)

## 🎥 Recording Sequence

### Scene 1: Extension Overview (30 seconds)
**Show:**
- Extension in VS Code marketplace
- Extension description and features
- Installation process

**Script:**
"Welcome to the Haskell Run extension demo! This extension brings Python-like simplicity to Haskell development in VS Code. With just one click, you can run entire Haskell files or individual functions."

### Scene 2: Basic File Execution (45 seconds)
**File:** `QuickStart.hs`
**Show:**
1. Open QuickStart.hs
2. Point out the ▶️ button in the editor toolbar
3. Click the ▶️ button to run the file
4. Show terminal output
5. Demonstrate F5 keyboard shortcut

**Script:**
"Let's start with basic file execution. I'll open QuickStart.hs and show the run button that appears in the editor. One click runs the entire file, just like Python! You can also use F5 as a keyboard shortcut."

### Scene 3: CodeLens Feature (60 seconds)
**File:** `QuickStart.hs`
**Show:**
1. Scroll through the file showing CodeLens ▶️ buttons above functions
2. Click CodeLens button for `hello` (no parameters)
3. Click CodeLens button for `addTwo` (with parameters)
4. Show parameter input dialog
5. Enter "5 3" and show REPL execution

**Script:**
"Notice the CodeLens run buttons above each function signature. These let you run individual functions directly. Functions without parameters run immediately, while functions with parameters show an input dialog where you can specify arguments."

### Scene 4: Functions TreeView (45 seconds)
**File:** `FunctionDemo.hs`
**Show:**
1. Open FunctionDemo.hs
2. Show the Haskell Run sidebar panel
3. Navigate through the Functions tree
4. Click on different functions to run them
5. Show function tooltips with signatures

**Script:**
"The Functions panel in the sidebar shows all functions in your current file. You can see function signatures as tooltips and execute any function with a single click. This gives you a bird's-eye view of your module's functionality."

### Scene 5: REPL Integration (90 seconds)
**File:** `InteractiveDemo.hs`
**Show:**
1. Open InteractiveDemo.hs
2. Run `greetUser` function with parameter "Alice"
3. Show REPL terminal with GHCi session
4. Run `simpleCalculator 10 5 "+"`
5. Run `generateSquares 8`
6. Demonstrate REPL persistence by running multiple functions
7. Show "Restart REPL" command in action

**Script:**
"Behind the scenes, the extension uses an integrated REPL powered by GHCi. When you run functions, they execute in a persistent REPL session. This means you can build up state and interact with your functions naturally. The REPL can be restarted or cleared as needed."

### Scene 6: Advanced Features (75 seconds)
**File:** `DataStructures.hs`
**Show:**
1. Open DataStructures.hs
2. Show complex data types and functions
3. Run `testPerson` function (shows custom data type)
4. Run `getName testPerson` (function with custom type parameter)
5. Run `area testShape` (pattern matching demonstration)
6. Show text selection + right-click menu for function execution
7. Demonstrate Shift+F5 keyboard shortcut for selected functions

**Script:**
"The extension handles complex Haskell features beautifully. Custom data types, pattern matching, and type signatures all work seamlessly. You can select any function name and run it using the right-click menu or Shift+F5."

### Scene 7: Code Snippets (30 seconds)
**File:** Create new file
**Show:**
1. Create a new .hs file
2. Type `hmain` and show snippet expansion
3. Type `hdata` and show data type snippet
4. Type `hclass` and show type class snippet

**Script:**
"The extension includes helpful code snippets for common Haskell patterns. Type 'hmain' for a main function, 'hdata' for data types, and 'hclass' for type classes."

### Scene 8: Configuration & Settings (45 seconds)
**Show:**
1. Open VS Code settings
2. Search for "Haskell Run"
3. Show available configuration options:
   - Default runner (runghc, stack runghc, cabal run)
   - Terminal reuse settings
   - Timeout settings
   - CodeLens enable/disable

**Script:**
"The extension is highly configurable. You can choose your preferred Haskell runner, configure timeouts, and customize the behavior to match your development workflow."

### Scene 9: Keyboard Shortcuts Summary (30 seconds)
**Show:**
1. Display keyboard shortcuts overlay or reference card
2. Quickly demonstrate each shortcut:
   - F5: Run file
   - Shift+F5: Run selected function
   - Ctrl+Alt+R: Run file (alternative)
   - Ctrl+Alt+F: Run function (alternative)
   - Ctrl+Alt+K: Restart REPL
   - Ctrl+Alt+L: Clear REPL

**Script:**
"Here's a quick reference for all keyboard shortcuts. These shortcuts work across all platforms with appropriate key mappings for Mac users."

### Scene 10: Conclusion (30 seconds)
**Show:**
1. Quick montage of all features
2. Extension marketplace page
3. GitHub repository link

**Script:**
"The Haskell Run extension transforms your Haskell development experience in VS Code. Whether you're learning Haskell or building complex applications, this extension provides the instant feedback and seamless execution you need. Try it today from the VS Code marketplace!"

## 🎯 Key Demonstration Points

### Must-Show Features:
1. ✅ One-click file execution (▶️ button)
2. ✅ CodeLens inline run buttons
3. ✅ Functions TreeView panel
4. ✅ REPL integration and persistence
5. ✅ Parameter input for functions
6. ✅ Text selection execution
7. ✅ Keyboard shortcuts (F5, Shift+F5)
8. ✅ Code snippets (hmain, hdata, hclass)
9. ✅ Configuration options
10. ✅ Custom data types handling

### Pro Tips for Recording:
- Keep cursor movements smooth and deliberate
- Pause briefly after each action to let viewers absorb
- Use larger font sizes for better visibility
- Ensure terminal output is clearly visible
- Test all demo functions before recording
- Have backup files ready in case of issues

## 📁 Demo Files Summary:
- **QuickStart.hs**: Perfect first demo, shows all basic features
- **FunctionDemo.hs**: Great for CodeLens and parameter demonstration
- **InteractiveDemo.hs**: Excellent for REPL features
- **DataStructures.hs**: Shows advanced type handling
- **AdvancedDemo.hs**: For power users and complex features
- **BasicDemo.hs**: Simple fallback demo file

## 🚀 Ready to Record!
Your demo files are perfectly structured to showcase every feature of the Haskell Run extension. Each file builds on the previous one, creating a comprehensive demonstration that will help users understand the full power of your extension.

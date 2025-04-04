#!/usr/bin/env node

const fs = require('fs');
const path = require('path');

/**
 * Converts markdown files to MDX by:
 * 1. Replacing the first line with new content
 * 2. Adding a new line after the first line
 * 3. Finding the page title (# Title) and adding new content right after it
 * 4. Adding a new line after the title line
 * 5. Changing the file extension to .mdx
 * 
 * @param {string} filePath - Path to the markdown file
 * @param {string} firstLineText - Text to insert as the first line
 * @param {string} afterTitleText - Text to insert after the page title
 */
function convertMarkdownToMdx(filePath, firstLineText, afterTitleText) {
  try {
    // Check if file exists
    if (!fs.existsSync(filePath)) {
      console.error(`File not found: ${filePath}`);
      return;
    }

    // Read the file content
    const content = fs.readFileSync(filePath, 'utf8');
    
    // Split content into lines
    const lines = content.split('\n');
    
    // Create new content array
    let newLines = [];

    // Replace the first line with firstLineText and add a new empty line after it
    newLines.push(firstLineText);
    newLines.push('');  // Adding an empty line after the first line

    // Process remaining lines to find title and insert text after it
    let titleFound = false;

    for (let i = 0; i < lines.length; i++) {
      const line = lines[i];

      // Add the current line to newLines
      newLines.push(line);

      // Check if this line is a markdown title (starts with # )
      if (!titleFound && line.trim().match(/^#\s+.+/)) {
        titleFound = true;
        // Insert the after-title text after the title
        newLines.push(afterTitleText);
        newLines.push('');  // Adding an empty line after the title
      }
    }

    // Join lines back together
    const newContent = newLines.join('\n');
    
    // Generate new filename with .mdx extension
    const dirName = path.dirname(filePath);
    const baseName = path.basename(filePath, path.extname(filePath));
    const newFilePath = path.join(dirName, `${baseName}.mdx`);
    
    // Write to the new file
    fs.writeFileSync(newFilePath, newContent);
    
    // If the new filename is different from the original (not just overwriting with same name)
    if (newFilePath !== filePath) {
      // Delete the original file
      fs.unlinkSync(filePath);
    }
    
    console.log(`Successfully converted ${filePath} to ${newFilePath}`);
  } catch (error) {
    console.error(`Error processing file ${filePath}:`, error);
  }
}

/**
 * Process multiple files or an entire directory
 * @param {string|string[]} input - File path, array of file paths, or directory path
 * @param {string} firstLineText - Text to insert as the first line
 * @param {string} afterTitleText - Text to insert after the page title
 * @param {boolean} recursive - Whether to process subdirectories recursively
 */
function processInput(input, firstLineText, afterTitleText, recursive = false) {
  // Handle array of files
  if (Array.isArray(input)) {
    input.forEach(file => convertMarkdownToMdx(file, firstLineText, afterTitleText));
    return;
  }
  
  // Check if input is a directory
  const stats = fs.statSync(input);
  if (stats.isDirectory()) {
    const files = fs.readdirSync(input);
    files.forEach(file => {
      const filePath = path.join(input, file);
      const fileStats = fs.statSync(filePath);
      
      if (fileStats.isFile() && path.extname(filePath) === '.md') {
        convertMarkdownToMdx(filePath, firstLineText, afterTitleText);
      } else if (recursive && fileStats.isDirectory()) {
        processInput(filePath, firstLineText, afterTitleText, recursive);
      }
    });
  } else if (path.extname(input) === '.md') {
    // Process a single file
    convertMarkdownToMdx(input, firstLineText, afterTitleText);
  } else {
    console.error(`${input} is not a markdown file or directory`);
  }
}

// Main execution
function main() {
  // Parse command-line arguments
  const args = process.argv.slice(2);
  
  if (args.length < 3) {
    console.log(`
    Usage: node ${path.basename(__filename)} <file-or-directory> <first-line-text> <after-title-text> [--recursive]
    
    Arguments:
      file-or-directory  - Path to markdown file or directory containing markdown files
      first-line-text    - Text to insert at the beginning of the file
      after-title-text   - Text to insert after the page title (# Title)
      --recursive        - (Optional) Process subdirectories recursively
    
    Example:
      node ${path.basename(__filename)} ./docs "import Component from '@site/src/components/Component';" "import SecondComponent from '@site/src/components/SecondComponent';" --recursive
    `);
    process.exit(1);
  }
  
  const inputPath = args[0];
  const firstLineText = args[1];
  const afterTitleText = args[2];
  const recursive = args.includes('--recursive');
  
  processInput(inputPath, firstLineText, afterTitleText, recursive);
}

// Run the script
main();

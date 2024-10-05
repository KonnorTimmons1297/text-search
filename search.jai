#import "Basic";
#import "File";
#import "File_Utilities";
#import "String";

Logical :: bool;

LINES_TO_PEEK :: 2;

IGNORED_EXTENSIONS := string.[
	".exe", ".pdb", ".lib", ".obj", ".exp"
];

main :: () {
	subdirs := false;
	diagnostics := false;
	peek := false;
	string_to_search_for: string;
	
	for arg, index: get_command_line_arguments() {
		if index == 0 continue;
		
		if begins_with(arg, "-subdirs") {
			subdirs = true;
		} else if begins_with(arg, "-diag") {
			diagnostics = true;
		} else if begins_with(arg, "-peek") {
			peek = true;
		} else {
			string_to_search_for = arg;
		}
	}
	
	if string_to_search_for.count == 0 {
		print("Usage: search SEARCH_TERM\n");
		print("Options:\n");
		print("   -subdirs: Search subdirectories\n");
		print("   -peek: Print lines before and after match\n");
		print("   -diag: Diagnostics Logging\n");
		return;
	}
	
	current_directory := get_working_directory();
	
	if diagnostics {
		log("Query: %", string_to_search_for);
		log("Directory: %\n", current_directory);
	}
	
	SearchForString(current_directory, string_to_search_for, subdirs, peek, diagnostics);
}

SearchArgs :: struct {
	string_to_search_for: string;
	print_diagnostics: Logical;
	peek: Logical;
	check_subdirs: Logical;
};

SearchFileVisitor :: (info: *File_Visit_Info, args: SearchArgs) {
	if info.had_error {
		log_error("Error opening file: %", info.short_name);
		return;
	}
	
	for ignored_extension: IGNORED_EXTENSIONS {
		if ends_with(info.short_name, ignored_extension) {
			if args.print_diagnostics {
				log("Ignoring file %", info.short_name);
			}
			return;
		}
	}
	
	if info.is_directory {
		info.descend_into_directory = args.check_subdirs;
	} else if info.is_symlink {
		if args.print_diagnostics {
			log("Ignoring symlink %", info.short_name);
		}
	} else {
		FindStringInFile(info.full_name, args);
	}
}

FindStringInFile :: (
	filepath: string, args: SearchArgs) {
	string_to_search_for := args.string_to_search_for;

	file_data, read_entire_file_succeeded := read_entire_file(filepath);
	if !read_entire_file_succeeded {
		log_error("Failed to read %", filepath);
		return;
	}
	
	reported_filename := false;
	line_number := 1; // all files have at least 1 line in them
	reported_lines: [..]int;
	reported_lines.allocator = __temporary_allocator;

	matched_byte_count := 0; // index into search query

	for file_data_index: 0..file_data.count-1 {
		byte := file_data[file_data_index];
		
		// need to know the current line number if match found
		if (byte == #char "\n") line_number += 1;
		
		// restart matching if a non-matching byte is encountered
		if string_to_search_for[matched_byte_count] != byte {
			matched_byte_count = 0;
			continue; // continue to next file byte to see if there is a match there
		}
		
		// at this point, we have a started a match
		matched_byte_count += 1;
		
		report_match := matched_byte_count == string_to_search_for.count;
		for reported_line: reported_lines {
			if line_number == reported_line {
				report_match = false;
				matched_byte_count = 0; // continue to next match
				break;
			}
		}
		
		if report_match {
			// only report the filename once, there can be more 
			// than one match in a file.
			if !reported_filename {
				print("####\n");
				print("#### %\n", filepath);
				print("####\n");
				reported_filename = true;
			}
			
			match_start_index := file_data_index - (string_to_search_for.count-1);
			matched_line := GetLineThatContainsIndex(file_data, match_start_index);
			
			if args.peek {
				print("========================\n");
				previous_lines := GetLinesBeforeIndex(file_data, match_start_index, LINES_TO_PEEK);
				previous_line_index := previous_lines.count-1;
				while previous_line_index >= 0 {
					previous_line := previous_lines[previous_line_index];
					previous_line_number := line_number - (previous_line_index+1);
					print("    L%: %\n", previous_line_number, previous_line);
					previous_line_index -= 1;
				}
				
				print("==> L%: %\n", line_number, matched_line);
				
				next_lines := GetLinesAfterIndex(file_data, match_start_index, LINES_TO_PEEK);
				for next_line_index: 0..next_lines.count-1 {
					next_line := next_lines[next_line_index];
					next_line_number := line_number + (next_line_index+1);
					print("    L%: %\n", next_line_number, next_line);
				}
				print("========================\n");
			} else {
				print("L%: %\n", line_number, matched_line);
			}
			
			// allow for multiple matches to be found in the same file
			matched_byte_count = 0;
			array_add(*reported_lines, line_number);
		}
	}
}

SearchForString :: (
	current_directory: string,
	string_to_search_for: string, 
	check_subdirs: Logical = false, 
	peek: Logical = false, 
	print_diagnostics: Logical = false) {
	
	args := SearchArgs.{
		string_to_search_for,
		print_diagnostics,
		peek,
		check_subdirs
	};

	visit_files(
		current_directory, 
		args.check_subdirs, 
		args, 
		SearchFileVisitor,
		visit_directories = args.check_subdirs);
}

BeginningOfLineIndex :: (str: string, starting_index: s64) -> s64 {
	if starting_index == 0 return 0;
	while starting_index > 0 {
		if str[starting_index-1] == #char "\n" {
			break;
		}
		starting_index -= 1;
	}
	return starting_index;
}

GetLineThatContainsIndex :: (buffer: string, index: int) -> string {
	line_start_index := BeginningOfLineIndex(buffer, index);
	line_end_index := EndOfLineIndex(buffer, index);
	line := GetLine(buffer, line_start_index, line_end_index);
	return line;
}

GetLinesBeforeIndex :: (buffer: string, index: int, lines_to_get: int) -> []string {
	lines: [..]string;
	lines.allocator = __temporary_allocator;
	
	beginning_of_current_line := BeginningOfLineIndex(buffer, index);
	
	for i: 0..lines_to_get-1 {
		if (beginning_of_current_line-2) < 0 break;
		end_of_previous_line := beginning_of_current_line - 2;
		start_of_previous_line := BeginningOfLineIndex(buffer, end_of_previous_line);
		previous_line := GetLine(buffer, start_of_previous_line, end_of_previous_line);
		array_add(*lines, previous_line);
		beginning_of_current_line = start_of_previous_line;
	}
	
	return lines;
}

GetLinesAfterIndex :: (buffer: string, index: int, lines_to_get: int) -> []string {
	lines: [..]string;
	lines.allocator = __temporary_allocator;
	
	end_of_current_line := EndOfLineIndex(buffer, index);
	
	for i: 0..lines_to_get-1 {
		if (end_of_current_line+2) < buffer.count break;
		start_of_next_line := end_of_current_line + 2;
		end_of_next_line := EndOfLineIndex(buffer, start_of_next_line);
		next_line := GetLine(buffer, start_of_next_line, end_of_next_line);
		array_add(*lines, next_line);
		end_of_current_line = end_of_next_line;
	}
	
	return lines;
}

EndOfLineIndex :: (str: string, starting_index: s64) -> s64 {
	if starting_index == str.count-1 return str.count-1;
	while starting_index < str.count-1 {
		if str[starting_index + 1] == #char "\n" {
			break;
		}
		starting_index += 1;
	}
	return starting_index;
}

GetLine :: (str: string, begin_index: s64, end_index: s64) -> string {
	line: string;
	line.data = str.data + begin_index;
	line.count = end_index - begin_index + 1;
	return line;
}

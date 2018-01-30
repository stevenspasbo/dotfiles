#-------------------------------------------------------------
# Author: Steven Spasbo
#-------------------------------------------------------------

#-------------------------------------------------------------
# Constants
#-------------------------------------------------------------
HOME = ENV['HOME']
REPO = File.expand_path(File.dirname(__FILE__))
BACKUP_DIR = REPO + "/backups"
REPO_FONT_DIR = REPO + "/fonts"
USER_FONT_DIR = "#{ENV['HOME']}/Library/Fonts"
TIME = Time.new
DATE_TIME_STRING = "#{TIME.month}_#{TIME.day}_#{TIME.year}_-_#{TIME.hour}_#{TIME.min}_#{TIME.sec}"

RVM = "#{ENV['HOME']}/.rvm"
HOMEBREW = "/usr/local/bin/brew"

#-------------------------------------------------------------
# Variables
#-------------------------------------------------------------
files = Dir.glob("dotfiles/*")

#-------------------------------------------------------------
# Methods
#-------------------------------------------------------------
def backup(file)
  new_file_name = "#{File.expand_path(BACKUP_DIR)}/#{File.basename(file)}#{DATE_TIME_STRING}"
  begin
    Dir.mkdir(BACKUP_DIR) unless Dir.exists?(BACKUP_DIR)
    unless File.exists?(new_file_name)
      File.rename(file, new_file_name)
    else
      File.rename(file, "#{new_file_name}#{rand(10000)}")
    end
  rescue SystemCallError => e
    puts e.message
  end
  puts "\t#{file} was moved to #{new_file_name}"
end

def is_mac?
  (RUBY_PLATFORM =~ /darwin/) != nil
end

def file_already_linked?(symlinked_file_in_home, repo_file)
    File.readlink(symlinked_file_in_home) == File.expand_path(repo_file)
end

def app_installed?(app)
  File.exists? app
end

#-------------------------------------------------------------
# Tasks
#-------------------------------------------------------------
task :default do
  puts "Run 'rake -T' to see the list of available tasks"
end

desc "Installs all dotfiles"
task :install_dotfiles do
  files.each do |file|
    home_file = "#{HOME}/.#{File.basename(file)}"
    base_dot_name = File.basename(home_file)
    puts "Creating symlink for #{base_dot_name}" unless File.exists?(home_file)
    backup home_file if (File.exists?(home_file) && !File.symlink?(home_file))
    File.delete home_file if File.symlink? home_file
    File.symlink("#{File.basename(Dir.pwd)}/#{file}", home_file)
  end
end

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

desc "Updates all the vim plugins"
task :update_vim do
  sh "git submodule foreach git pull origin master"
end

desc "Installs all dotfiles"
task :install_dotfiles do
  files.each do |file|
    home_file = "#{HOME}/.#{File.basename(file)}"
    base_dot_name = File.basename(home_file)
    puts "Creating symlink for #{base_dot_name}" unless File.exists?(home_file)
    backup home_file if (File.exists?(home_file) && !File.symlink?(home_file))
    File.delete home_file if File.symlink? home_file
    File.symlink(File.expand_path(file), home_file)
  end
end

desc "Installs RVM"
task :install_rvm do
  unless app_installed? RVM 
    print "Install RVM? (y/n) "
    ans = $stdin.gets.downcase.chomp
    if (ans == "y" || ans == "yes")
      sh "curl -sSL https://get.rvm.io | bash"
    end
  else
    puts "RVM already installed"
  end
end

desc "Installs Homebrew"
task :install_homebrew do
  unless is_mac?
    puts "Mac only"
    break
  end
  unless app_installed? HOMEBREW
    print "Install Homebrew? (y/n) "
    ans = $stdin.gets.downcase.chomp
    if (ans == "y" || ans == "yes")
      sh 'ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"'
    end
  else
    puts "Homebrew already installed"
  end
end

desc "Installs everything"
task :install_all => [ :install_homebrew, :install_fonts, :install_rvm, :install_dotfiles ] do
  puts "Installing everything..."
end

desc "Installs fonts"
task :install_fonts do
  if (is_mac?)
    Dir["#{REPO_FONT_DIR}/*"].each do |font|
      unless (File.exists? "#{USER_FONT_DIR}/#{File.basename(font)}")
        FileUtils.cp(font, USER_FONT_DIR)
      else
        puts "#{File.basename(font)} already exists in Users library"
      end
    end
    puts "Fonts now installed." 
  else
    puts "Sorry, mac only feature right now."
  end
end

require 'colorize'

#-------------------------------------------------------------
# Tasks
#-------------------------------------------------------------
home_dir = ENV['HOME']

task :default do
  puts "Rakefile for my dotfiles. WIP. (default task)"
end

desc "Installs all dotfiles"
task :install do

end

desc "Installs bash files"
task :bashrc do
  puts "Installing bashrc"
end

desc "Installs vim files"
task :vim do
  vimrc = "#{home_dir}/.vimrc"
  vim_dir = "#{home_dir}/.vim"
  if File.exists?(vimrc)
    puts "[ #{File.basename(vimrc)} found ]".colorize(:red)
    unless File.symlink?(vimrc)
      backup(vimrc)
      puts "\tMoved to $repoLocation/backup/ "
    else
      puts "\tFile is symlink, skipping backup "
    end
  else
    puts "[ No .vimrc file found ]"
    File.symlink("#{Dir.pwd}/vimrc", vimrc)
    puts "[ * ~/.vimrc symlink created ]"
  end
end

desc "Updates all vim plugins"
task :update_vim_plugins => :vim do

end

desc "Backs up dotfiles to $REPO/Backup"
task :backup do
  Dir.entries.each do |file|
    unless rakefile?(file) && markdown?(file)

end

#-------------------------------------------------------------
# Methods
#-------------------------------------------------------------

def backup(file)
  unless File.symlink?(file)
    t = Time.new
    date_str = "#{t.month}_#{t.day}_#{t.year}_-_#{t.hour}_#{t.min}_#{t.sec}"
    begin
      Dir.mkdir("Backup/"date_str)
      Dir.chdir(date_str)
      mv(file, "#{Dir.pwd}/backup/#{new_file_name}")
    rescue SystemCallError -> e
      puts e.message
      puts e.backtrace.inspect
  end
end

def rakefile?(file)
  File.expand_path(file) == File.expand_path(__FILE__)
end

def markdown?(file)
  File.extname(file).downcase == ".markdown"
end

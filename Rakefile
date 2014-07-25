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
  puts "OMG EVERYTHING IS DESTROYED"
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
  #if Dir.exists?(vim_dir)
  #  puts "[ #{File.basename(vimrc)} found ]"
  #  unless File.symlink?(vimrc)
  #    #backup(vimrc)
  #    puts "\tMoved to $repoLocation/backup/ "
  #  else
  #    puts "\tFile is symlink, skipping backup "
  #  end
  #else
  #  #ln_s("#{Dir.pwd}/vimrc", vimrc)
  #end
end

desc "Updates all vim plugins"
task :update_vim_plugins => :vim do

end

#-------------------------------------------------------------
# Methods
#-------------------------------------------------------------

def backup(file)
  unless File.symlink?(file)
    t = Time.new
    new_file_name = "#{file}_#{t.month}_#{t.day}_#{t.year}"
    mv(file, "#{Dir.pwd}/backup/#{new_file_name}")
  end
end



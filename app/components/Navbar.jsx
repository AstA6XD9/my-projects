"use client"
import React, { useRef, useState, useEffect } from 'react'
import Image from 'next/image'
import { motion } from 'framer-motion'
import { assets } from '../../assets/assets'

export default function Navbar({ isDarkMode, setIsDarkMode }) {
  const sideMenuRef = useRef(null);
  const [isScroll, setIsScroll] = useState(false);
  const [isMenuOpen, setIsMenuOpen] = useState(false);

  const openMenu = () => {
    setIsMenuOpen(true);
  }

  const closeMenu = () => {
    setIsMenuOpen(false);
  }

  useEffect(() => {
    const handleScroll = () => {
      if (window.scrollY > 50) {
        setIsScroll(true);
      } else {
        setIsScroll(false);
      }
    };

    window.addEventListener('scroll', handleScroll);
    return () => window.removeEventListener('scroll', handleScroll);
  }, []);

  const toggleDarkMode = () => {
    setIsDarkMode(!isDarkMode);
  };

  return (
    <>
      {!isDarkMode && (
        <div className='fixed top-0 right-0 w-11/12 -z-10 translate-y-[-80%]'> 
          <Image src={assets.header_bg_color} alt='' className='w-full' />
        </div>
      )}
      <motion.nav 
        initial={{ y: -20, opacity: 0 }}
        animate={{ y: 0, opacity: 1 }}
        transition={{ duration: 0.4, ease: "easeOut" }}
        className={`w-full fixed px-5 lg:px-8 xl:px-[8%] py-4 flex items-center justify-between z-50 ${isScroll ? (isDarkMode ? "bg-gray-900 bg-opacity-90 backdrop-blur-lg shadow-sm" : "bg-gray-50 bg-opacity-90 backdrop-blur-lg shadow-sm") : ""} ${isDarkMode ? 'bg-gray-900 text-white' : 'bg-gray-50 text-black'}`}
      >
        <motion.a 
          href="#top"
          initial={{ opacity: 0 }}
          animate={{ opacity: 1 }}
          transition={{ duration: 0.3, delay: 0.1 }}
        >
          <Image 
            src={isDarkMode ? assets.logo_dark : assets.logo} 
            className='w-28 cursor-pointer mr-10' 
            alt="Logo" 
          />
        </motion.a>
        <motion.ul 
          initial={{ opacity: 0 }}
          animate={{ opacity: 1 }}
          transition={{ duration: 0.3, delay: 0.2 }}
          className={`hidden md:flex items-center gap-6 lg:gap-8 rounded-full px-12 py-3 ${isScroll ? "" : isDarkMode ? "bg-gray-800 shadow-sm bg-opacity-50" : "bg-gray-100 shadow-sm bg-opacity-50"} `}
        >
          <li><a className={`font-Ovo hover:underline transition-all duration-300 ${isDarkMode ? 'text-white hover:text-gray-300' : 'text-black hover:text-gray-600'}`} href="#top">Home</a></li>
          <li><a className={`font-Ovo hover:underline transition-all duration-300 ${isDarkMode ? 'text-white hover:text-gray-300' : 'text-black hover:text-gray-600'}`} href="#about">About me</a></li>
          <li><a className={`font-Ovo hover:underline transition-all duration-300 ${isDarkMode ? 'text-white hover:text-gray-300' : 'text-black hover:text-gray-600'}`} href="#projects">Projects</a></li>
          <li><a className={`font-Ovo hover:underline transition-all duration-300 ${isDarkMode ? 'text-white hover:text-gray-300' : 'text-black hover:text-gray-600'}`} href="#inspiration">Inspiration</a></li>
          <li><a className={`font-Ovo hover:underline transition-all duration-300 ${isDarkMode ? 'text-white hover:text-gray-300' : 'text-black hover:text-gray-600'}`} href="#contact">Contact</a></li>
        </motion.ul>
        <motion.div 
          initial={{ opacity: 0 }}
          animate={{ opacity: 1 }}
          transition={{ duration: 0.3, delay: 0.3 }}
          className='flex items-center gap-4'
        >
          <button className='p-2 hover:bg-gray-200 dark:hover:bg-gray-700 rounded-lg transition-colors duration-300' onClick={toggleDarkMode}>  
            <Image src={isDarkMode ? assets.sun_icon : assets.moon_icon} alt={isDarkMode ? 'Light Mode' : 'Dark Mode'} className='w-6' />
          </button>
          <a href="#contact" className={`hidden lg:flex items-center gap-3 px-10 py-2.5 border rounded-full ml-4 font-Ovo transition-all duration-300 ${isDarkMode ? 'border-gray-400 text-white hover:bg-gray-800' : 'border-gray-500 text-black hover:bg-gray-100'}`}>
            Contact <Image src={assets.arrow_icon} className='w-3' alt="Arrow" />
          </a>
          <button className='block md:hidden m-3 p-2 hover:bg-gray-200 dark:hover:bg-gray-700 rounded-lg transition-colors duration-300' onClick={openMenu}> 
            <Image src={isDarkMode ? assets.menu_white : assets.menu_black} alt='Menu' className='w-6' /> 
          </button>
        </motion.div>

        {/* Mobile menu overlay */}
        {isMenuOpen && (
          <div 
            className="fixed inset-0 bg-black bg-opacity-50 z-40 md:hidden"
            onClick={closeMenu}
          />
        )}
        
        {/* Mobile menu */}
        <ul ref={sideMenuRef} className={`flex md:hidden flex-col gap-6 py-20 px-10 fixed right-0 top-0 bottom-0 w-64 z-50 h-screen transition duration-500 transform shadow-2xl ${isMenuOpen ? 'translate-x-0' : 'translate-x-full'} ${isDarkMode ? 'bg-gray-900 text-white' : 'bg-gray-50 text-black'}`}>
          <div className='absolute right-6 top-6' onClick={closeMenu}>
            <Image src={isDarkMode ? assets.close_white : assets.close_black} alt='Close' className='w-5 cursor-pointer'/>
          </div>  
          <li><a className={`font-Ovo hover:underline transition-all duration-300 ${isDarkMode ? 'text-white hover:text-gray-300' : 'text-black hover:text-gray-600'}`} onClick={closeMenu} href="#top">Home</a></li>
          <li><a className={`font-Ovo hover:underline transition-all duration-300 ${isDarkMode ? 'text-white hover:text-gray-300' : 'text-black hover:text-gray-600'}`} onClick={closeMenu} href="#about">About me</a></li>
          <li><a className={`font-Ovo hover:underline transition-all duration-300 ${isDarkMode ? 'text-white hover:text-gray-300' : 'text-black hover:text-gray-600'}`} onClick={closeMenu} href="#projects">Projects</a></li>
          <li><a className={`font-Ovo hover:underline transition-all duration-300 ${isDarkMode ? 'text-white hover:text-gray-300' : 'text-black hover:text-gray-600'}`} onClick={closeMenu} href="#inspiration">Inspiration</a></li>
          <li><a className={`font-Ovo hover:underline transition-all duration-300 ${isDarkMode ? 'text-white hover:text-gray-300' : 'text-black hover:text-gray-600'}`} onClick={closeMenu} href="#contact">Contact</a></li>
        </ul>
      </motion.nav>
    </>
  )
}

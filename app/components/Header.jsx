import React from "react";
import Image from "next/image";
import { motion } from "framer-motion";
import { assets } from "../../assets/assets";

export default function Header({ isDarkMode }) {
    return (
        <motion.div 
          initial={{ opacity: 0 }}
          animate={{ opacity: 1 }}
          transition={{ duration: 0.6 }}
          className={`w-11/12 max-w-3xl text-center mx-auto min-h-screen pt-24 md:pt-28 flex flex-col items-center justify-center gap-4 ${isDarkMode ? 'bg-gray-900 text-white' : 'bg-gray-50 text-black'}`}
        >
            <motion.div
              initial={{ opacity: 0 }}
              animate={{ opacity: 1 }}
              transition={{ duration: 0.4, delay: 0.1 }}
            >
                <Image src = {assets.profile_img} alt ='' className='rounded-full w-48 sm:w-56 lg:w-64' />
            </motion.div>
            
            <motion.h3 
              initial={{ y: 20, opacity: 0 }}
              animate={{ y: 0, opacity: 1 }}
              transition={{ duration: 0.4, delay: 0.2 }}
              className={`flex items-end gap-2 text-xl md:text-2xl mb-3 font-Ovo ${isDarkMode ? 'text-white' : 'text-gray-800'}`}
            >
                Hi! I'm M-Amine El Ouardini <Image src = {assets.hand_icon} alt='' className = 'w-6' />
            </motion.h3>
            
            <motion.h1 
              initial={{ y: 20, opacity: 0 }}
              animate={{ y: 0, opacity: 1 }}
              transition={{ duration: 0.4, delay: 0.3 }}
              className={`text-3xl sm:text-6xl lg:text-[66px] font-Ovo ${isDarkMode ? 'text-white' : 'text-gray-800'}`}
            >
                Image Processing and Multimedia student at ENSEEIHT
            </motion.h1>
            
            <motion.p 
              initial={{ y: 20, opacity: 0 }}
              animate={{ y: 0, opacity: 1 }}
              transition={{ duration: 0.4, delay: 0.4 }}
              className={`max-w-2xl mx-auto font-Ovo text-base sm:text-lg mt-3 ${isDarkMode ? 'text-gray-300' : 'text-gray-700'}`}
            >
                I'm an image processing student and I'm passionate and curious about exploring how visuals, design, and technology can create impactful multimedia experiences.
            </motion.p>
            
            <motion.div 
              initial={{ y: 20, opacity: 0 }}
              animate={{ y: 0, opacity: 1 }}
              transition={{ duration: 0.4, delay: 0.5 }}
              className="flex flex-col sm:flex-row items-center gap-4 mt-4"
            >
                <a href="#contact"
                className="px-5 py-1 border border-white rounded-full bg-black text-white items-center flex gap-2 font-Ovo" >Contact me <Image src = {assets.right_arrow_white} alt ='' className='rounded-full w-4'/></a>

                <a href="/sample-resume.pdf" download className ='px-5 py-1 border rounded-full border-gray-500 flex items-center gap-2 font-Ovo' >My resume <Image src = {assets.download_icon} alt ='' className='rounded-full w-4'/></a>
            </motion.div>
        </motion.div>
    )
}
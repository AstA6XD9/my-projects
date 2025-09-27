import React from "react";
import { assets } from "@/assets/assets";
import Image from "next/image";
import { motion } from "framer-motion";

const Inspiration = ({ isDarkMode }) => {
  return (
    <motion.div 
      initial={{ opacity: 0 }}
      whileInView={{ opacity: 1 }}
      transition={{ duration: 0.8 }}
      viewport={{ once: true }}
      id="inspiration" 
      className={`w-full px-[12%] py-20 scroll-mt-20 ${isDarkMode ? 'bg-gray-900 text-white' : 'bg-gray-50 text-black'}`}
    >
      <div className="max-w-6xl mx-auto">
        {/* Header Section */}
        <motion.div 
          initial={{ y: 30, opacity: 0 }}
          whileInView={{ y: 0, opacity: 1 }}
          transition={{ duration: 0.6 }}
          viewport={{ once: true }}
          className="text-center mb-16"
        >
          <h4 className="text-lg font-Ovo mb-4">
            My Aspiration
          </h4>
          <h2 className="text-5xl font-Ovo mb-6">
            Passion for Visual Storytelling
          </h2>
          <p className="text-lg max-w-3xl mx-auto leading-relaxed">
            As a student in Image and Multimedia, I believe in the power of visual communication 
            to transform ideas into compelling experiences that connect with people.
          </p>
        </motion.div>

        {/* Main Quote Section - Creative Design */}
        <motion.div 
          initial={{ y: 20, opacity: 0 }}
          whileInView={{ y: 0, opacity: 1 }}
          transition={{ duration: 0.4, delay: 0.1 }}
          viewport={{ once: true }}
          className="bg-gradient-to-br from-white via-blue-50 to-purple-50 rounded-3xl shadow-2xl p-12 mb-16 relative overflow-hidden border border-blue-200"
        >
          {/* Animated Background Elements */}
          <div className="absolute top-0 right-0 w-40 h-40 bg-gradient-to-br from-blue-400/20 to-purple-400/20 rounded-full -translate-y-20 translate-x-20 animate-pulse"></div>
          <div className="absolute bottom-0 left-0 w-32 h-32 bg-gradient-to-tr from-purple-400/20 to-pink-400/20 rounded-full translate-y-16 -translate-x-16 animate-pulse delay-1000"></div>
          <div className="absolute top-1/2 left-1/2 w-24 h-24 bg-gradient-to-br from-green-400/20 to-blue-400/20 rounded-full -translate-x-12 -translate-y-12 animate-pulse delay-500"></div>
          
          {/* Quote Content */}
          <div className="relative z-10">
            {/* Creative Header */}
            <div className="flex items-center justify-center mb-8">
              <div className="w-20 h-1 bg-gradient-to-r from-blue-500 via-purple-500 to-pink-500 rounded-full"></div>
              <div className="mx-6 p-3 bg-gradient-to-br from-blue-100 to-purple-100 rounded-full">
                <Image src={assets.etoile} alt="Quote icon" className="w-8 h-8" />
              </div>
              <div className="w-20 h-1 bg-gradient-to-r from-pink-500 via-purple-500 to-blue-500 rounded-full"></div>
            </div>
            
            {/* Main Quote with Creative Typography */}
            <div className="relative">
              <blockquote className="text-2xl md:text-3xl lg:text-4xl font-Ovo text-center leading-relaxed mb-8 italic relative">
                <span className="text-6xl text-blue-500 absolute -top-4 -left-4">"</span>
                <span className="relative z-10">
                  Through algorithms, we can teach machines to see, imagine, and create.
                </span>
                <span className="text-6xl text-purple-500 absolute -bottom-4 -right-4">"</span>
              </blockquote>
              
              {/* Author Section with Creative Design */}
              <div className="text-center">
                <div className="inline-flex items-center gap-3 bg-gradient-to-r from-blue-500/10 to-purple-500/10 px-6 py-3 rounded-full">
                  <div className="w-2 h-2 bg-blue-500 rounded-full animate-pulse"></div>
                  <p className="text-lg font-semibold">M-Amine El Ouardini</p>
                  <div className="w-2 h-2 bg-purple-500 rounded-full animate-pulse delay-500"></div>
                </div>
                <p className="text-sm mt-2">Image Processing & Multimedia Student • ENSEEIHT</p>
              </div>
            </div>
          </div>
        </motion.div>

        {/* Visual Elements Grid */}
        <motion.div 
          initial={{ y: 20, opacity: 0 }}
          whileInView={{ y: 0, opacity: 1 }}
          transition={{ duration: 0.4, delay: 0.2 }}
          viewport={{ once: true }}
          className="grid grid-cols-1 md:grid-cols-3 gap-8 mb-16"
        >
          {/* Design Philosophy */}
          <motion.div 
            initial={{ y: 20, opacity: 0 }}
            whileInView={{ y: 0, opacity: 1 }}
            transition={{ duration: 0.4, delay: 0.3 }}
            viewport={{ once: true }}
            className={`relative rounded-xl p-8 shadow-lg hover:shadow-xl transition-all duration-300 hover:-translate-y-2 group overflow-hidden ${isDarkMode ? 'bg-gray-800' : 'bg-white'}`}
          >
            {/* Background Image */}
            <div className="absolute inset-0 opacity-20">
              <Image src={assets.creative_vision} alt="Creative Vision" className="w-full h-full object-cover rounded-xl" />
            </div>
            <div className={`absolute inset-0 opacity-30 ${isDarkMode ? 'bg-gray-900' : 'bg-white'}`}></div>
            
            <div className="relative z-10">
              <h3 className={`text-xl font-semibold mb-4 group-hover:text-blue-700 transition-colors duration-300 ${isDarkMode ? 'text-white' : 'text-gray-800'}`}>
                Creative Vision
              </h3>
              <p className={`leading-relaxed ${isDarkMode ? 'text-gray-300' : 'text-gray-600'}`}>
                Every pixel tells a story. I strive to create visual experiences that not only 
                look beautiful but also communicate effectively and inspire action.
              </p>
            </div>
          </motion.div>

          {/* Technical Excellence */}
          <motion.div 
            initial={{ y: 20, opacity: 0 }}
            whileInView={{ y: 0, opacity: 1 }}
            transition={{ duration: 0.4, delay: 0.4 }}
            viewport={{ once: true }}
            className={`relative rounded-xl p-8 shadow-lg hover:shadow-xl transition-all duration-300 hover:-translate-y-2 group overflow-hidden ${isDarkMode ? 'bg-gray-800' : 'bg-white'}`}
          >
            {/* Background Image */}
            <div className="absolute inset-0 opacity-20">
              <Image src={assets.technical} alt="Technical Mastery" className="w-full h-full object-cover rounded-xl" />
            </div>
            <div className={`absolute inset-0 opacity-30 ${isDarkMode ? 'bg-gray-900' : 'bg-white'}`}></div>
            
            <div className="relative z-10">
              <h3 className={`text-xl font-semibold mb-4 group-hover:text-purple-700 transition-colors duration-300 ${isDarkMode ? 'text-white' : 'text-gray-800'}`}>
                Technical Mastery
              </h3>
              <p className={`leading-relaxed ${isDarkMode ? 'text-gray-300' : 'text-gray-600'}`}>
                From concept to execution, I combine artistic creativity with technical precision 
                to deliver multimedia solutions that exceed expectations.
              </p>
            </div>
          </motion.div>

          {/* Future Goals */}
          <motion.div 
            initial={{ y: 20, opacity: 0 }}
            whileInView={{ y: 0, opacity: 1 }}
            transition={{ duration: 0.4, delay: 0.5 }}
            viewport={{ once: true }}
            className={`relative rounded-xl p-8 shadow-lg hover:shadow-xl transition-all duration-300 hover:-translate-y-2 group overflow-hidden ${isDarkMode ? 'bg-gray-800' : 'bg-white'}`}
          >
            {/* Background Image */}
            <div className="absolute inset-0 opacity-20">
              <Image src={assets.innovation} alt="Innovation Drive" className="w-full h-full object-cover rounded-xl" />
            </div>
            <div className={`absolute inset-0 opacity-30 ${isDarkMode ? 'bg-gray-900' : 'bg-white'}`}></div>
            
            <div className="relative z-10">
              <h3 className={`text-xl font-semibold mb-4 group-hover:text-green-700 transition-colors duration-300 ${isDarkMode ? 'text-white' : 'text-gray-800'}`}>
                Innovation Drive
              </h3>
              <p className={`leading-relaxed ${isDarkMode ? 'text-gray-300' : 'text-gray-600'}`}>
                I'm passionate about pushing boundaries in multimedia design, exploring new 
                technologies and creating immersive experiences that captivate audiences.
              </p>
            </div>
          </motion.div>
        </motion.div>

      </div>
    </motion.div>
  );
};

export default Inspiration;
